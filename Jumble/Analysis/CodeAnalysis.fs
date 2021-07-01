module Jumble.Analysis.CodeAnalysis

open System.Collections.Generic
open FSharpPlus
open Jumble
open Jumble.Analysis
open Mono.Cecil
open Mono.Cecil.Cil
open Serilog

type ModuleAnalysisInterim = {
    EnumToStringConversion: ResizeArray<TypeDefinition>
    FieldReferences: HashSet<FieldReference>
    TypeReferences: HashSet<TypeReference>
    MethodReferences: HashSet<MethodReference>
}

module ModuleAnalysisInterim =
    let create () =
        {
            EnumToStringConversion = ResizeArray()
            FieldReferences = HashSet()
            TypeReferences = HashSet()
            MethodReferences = HashSet()
        }

    let addMemberReference (an:ModuleAnalysisInterim) (m:MemberReference) =
        match m with
        | :? MethodReference as mr -> an.MethodReferences.Add(mr) |> ignore
        | :? FieldReference as fr -> an.FieldReferences.Add(fr) |> ignore
        | _ -> failwith $"Member reference {m.GetType().FullName} is not supported"


type ModuleAnalysisResult =
    {
        EnumToStringConversion: TypeDefinition array
        FieldReferences: IReadOnlyDictionary<FieldDefinition, FieldReference[]>
        TypeReferences: IReadOnlyDictionary<TypeDefinition, TypeReference[]>
        MethodReferences: IReadOnlyDictionary<MethodDefinition, MethodReference[]>
    }
    with
        member this.Lookups =
            let lookup (dict:IReadOnlyDictionary<'T, 'U array>) key =
                dict.GetValueOrDefault(key, Array.empty)
            {
                Lookups.TypeLookup = lookup this.TypeReferences
                MemberLookup = function
                               | :? TypeDefinition as td -> lookup this.TypeReferences td |> Seq.cast
                               | :? MethodDefinition as md -> lookup this.MethodReferences md |> Seq.cast
                               | :? FieldDefinition as fd -> lookup this.FieldReferences fd |> Seq.cast
                               | :? PropertyDefinition -> Seq.empty // is it really?
                               | :? EventDefinition -> Seq.empty // is it really
                               | m -> failwithf $"Member definition %s{m.GetType().Name} is not supported"
                MethodLookup = lookup this.MethodReferences
                FieldLookup = lookup this.FieldReferences }

module ModuleAnalysisResult =

    let mergeInterim (rslvr:Resolvers)  (xs:ModuleAnalysisInterim array) : ModuleAnalysisResult =
        let inline setToDict (r:_ -> _) xs = xs |> HashSet.merge |> Seq.toArray |> Array.groupBy r |> readOnlyDict

        let enumToStringConv = xs |> ResizeArray.collectArray (fun x -> x.EnumToStringConversion)
        let fieldReferences = xs |> Seq.map (fun x -> x.FieldReferences) |> setToDict rslvr.FieldResolver
        let methodReferences =
            xs
            |> Seq.map (fun x -> x.MethodReferences)
            |> HashSet.merge
            |> Seq.filter (fun m -> if m.DeclaringType.IsArray then Log.Warning("Filtering out method {Method}", m.FullName); false else true)
            |> Seq.toArray
            |> Array.groupBy rslvr.MethodResolver
            |> readOnlyDict
        let typeReferences = xs |> Seq.map (fun x -> x.TypeReferences) |> setToDict rslvr.TypeResolver

        {
            ModuleAnalysisResult.FieldReferences = fieldReferences
            EnumToStringConversion = enumToStringConv
            TypeReferences = typeReferences
            MethodReferences = methodReferences
        }

/// Extracts method reference from mr or its subtypes
let rec private deriveMethodReference (mr: MethodReference): MethodReference option =
    match mr with
    | :? MethodDefinition -> None
    | :? GenericInstanceMethod as gim -> deriveMethodReference gim.ElementMethod
    | _ when mr.GetType() = typeof<MethodReference> -> Some mr
    | _ -> failwithf $"%A{mr} is not supported"

let rec private deriveReference (mr: MemberReference): MemberReference option =
    match mr with
    | :? MethodReference as r -> deriveMethodReference r |> Option.map (fun x -> upcast x)
    | _ -> failwithf $"Reference type %s{mr.GetType().FullName} is not supported"

let private buildMethodAnalyser (rsvlr:Resolvers) (analysis:ModuleAnalysisInterim) =
    let analyseMethod (m:MethodDefinition) =
        // method body references
        if m.HasBody = false then () else
        let instrs = m.Body.Instructions

        for index = 0 to instrs.Count - 1 do
            let i1 = instrs.[index]

            match i1.OpCode.Code with
            | Code.Box | Code.Constrained ->
                // there should be always another instr after Box and Constrained
                let i2 = instrs.[index + 1]

                // fltEnumToString
                if i2.OpCode.Code = Code.Callvirt then
                    let callMethodRef = i2.Operand :?> MethodReference
                    if callMethodRef.Name = "ToString" && callMethodRef.DeclaringType.FullName = "System.Object" then
                        match i1.Operand with
                        | :? TypeReference as tr when tr.GetType() = typedefof<TypeReference> || tr.GetType() = typedefof<TypeDefinition> ->
                            let objRef = rsvlr.TypeResolver tr
                            assert (objRef <> null)
                            if objRef.IsEnum then
                                analysis.EnumToStringConversion.Add(objRef)
                        | _ -> ()

            | Code.Call
            | Code.Callvirt
            | Code.Ldftn
            | Code.Ldvirtftn ->
                i1.Operand :?> MethodReference |> deriveMethodReference |> Option.iter (fun mr -> analysis.MethodReferences.Add(mr) |> ignore)
            | Code.Ldfld
            | Code.Ldflda
            | Code.Ldsfld
            | Code.Ldsflda
            | Code.Stfld
            | Code.Stsfld ->
                i1.Operand :?> FieldReference |> (fun fr -> analysis.FieldReferences.Add(fr) |> ignore)
            | _ -> ()

        // overridden method references
        if m.Overrides <> null then
            for index = 0 to m.Overrides.Count - 1 do
                analysis.MethodReferences.Add(m.Overrides.[index]) |> ignore

    analyseMethod

let analyseModule (rslvr:Resolvers) (m:ModuleDefinition) =
    let an = ModuleAnalysisInterim.create()

    m.GetMemberReferences() |> Seq.iter (ModuleAnalysisInterim.addMemberReference an)

    let methodAnalyser = buildMethodAnalyser rslvr an
    let allTypes = ModuleDefinition.allTypes m |> Seq.toArray
    allTypes |> Seq.collect (fun t -> t.Methods) |> Seq.iter methodAnalyser
    an.TypeReferences.UnionWith(m.GetTypeReferences())
    an

let private analyseModules (rslvr:Resolvers) (rx:ModuleDefinition seq) =
    let f x = async { return analyseModule rslvr x }
    let res = rx |> Seq.map f |> Async.Parallel |> Async.RunSynchronously

    ModuleAnalysisResult.mergeInterim rslvr res

let private analyseAssembly (rslvr:Resolvers) (a:AssemblyDefinition) =
    a.Modules |> Seq.mapArray (analyseModule rslvr)

/// One-pass method body analyser.
let analyseAssemblies (rslvr:Resolvers) (xs:AssemblyDefinition seq) =
    let f (x:AssemblyDefinition) = async {
        return timeThisSeconds "Analysed assembly {Assembly:l}" [| x.Name.Name |] (fun () -> analyseAssembly rslvr x)
    }

    let interims = timeThisSeconds "Analysed all assemblies" Array.empty (fun () -> xs |> Seq.map f |> Async.Parallel |> Async.RunSynchronously |> Array.collect id)
    let res = timeThisSeconds "Merged code analysis results" Array.empty (fun () -> ModuleAnalysisResult.mergeInterim rslvr interims)

    res