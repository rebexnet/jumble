module Jumble.Analysis.CodeAnalysis

open System.Collections.Generic
open FSharpPlus
open Jumble
open Jumble.Analysis
open Mono.Cecil
open Mono.Cecil.Cil
open Serilog

type ReferenceAnalysisInterim = {
    FieldReferences: HashSet<FieldReference>
    TypeReferences: HashSet<TypeReference>
    MethodReferences: HashSet<MethodReference>
}

module ReferenceAnalysisInterim =
    let create () =
        {
            FieldReferences = HashSet()
            TypeReferences = HashSet()
            MethodReferences = HashSet()
        }

    let addMemberReference (an:ReferenceAnalysisInterim) (m:MemberReference) =
        match m with
        | :? MethodReference as mr -> an.MethodReferences.Add(mr) |> ignore
        | :? FieldReference as fr -> an.FieldReferences.Add(fr) |> ignore
        | _ -> failwith $"Member reference {m.GetType().FullName} is not supported"

type ReferenceAnalysisResult =
    {
        FieldReferences: IReadOnlyDictionary<FieldDefinition, FieldReference[]>
        TypeReferences: IReadOnlyDictionary<TypeDefinition, TypeReference[]>
        MethodReferences: IReadOnlyDictionary<MethodDefinition, MethodReference[]>
        MethodRIDReferences: IReadOnlyDictionary<MemberID, MethodReference[]>
        Assemblies: AssemblyCache
    }
    with
        member this.Lookups =
            let lookup (dict:IReadOnlyDictionary<'T, 'U array>) key =
                dict.GetValueOrDefault(key, Array.empty)
            {
                Lookups.TypeRefLookup = lookup this.TypeReferences
                MemberRefLookup = function
                               | :? TypeDefinition as td -> lookup this.TypeReferences td |> Seq.cast
                               | :? MethodDefinition as md -> lookup this.MethodReferences md |> Seq.cast
                               | :? FieldDefinition as fd -> lookup this.FieldReferences fd |> Seq.cast
                               | :? PropertyDefinition -> Seq.empty // is it really?
                               | :? EventDefinition -> Seq.empty // is it really
                               | :? MethodReference as mr ->
                                   let md = MethodReference.safeResolve mr
                                   lookup this.MethodReferences md |> Seq.cast
                               | m -> failwithf $"Member definition %s{m.GetType().Name} is not supported"
                MethodRefLookup = lookup this.MethodReferences
                FieldRefLookup = lookup this.FieldReferences
                MemberIDLookup = this.Assemblies.GetMember
                }

module ReferenceAnalysisResult =
    let mergeInterim (rslvr:Resolvers) (assemblyCache:AssemblyCache)  (xs:ReferenceAnalysisInterim array) : ReferenceAnalysisResult =
        let inline setToDict (r:_ -> _) xs = xs |> HashSet.merge |> Seq.toArray |> Array.groupBy r |> readOnlyDict

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
        let methodRIDReferences = methodReferences
                                  |> Seq.map (fun kvp -> (MemberID.fromDefinition kvp.Key, kvp.Value))
                                  |> readOnlyDict
        {
            ReferenceAnalysisResult.FieldReferences = fieldReferences
            TypeReferences = typeReferences
            MethodReferences = methodReferences
            Assemblies = assemblyCache
            MethodRIDReferences = methodRIDReferences
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

module EnumToStringConversionAnalyser =

    let private findInMethod (typeResolver:TypeResolver) (m:MethodDefinition) : TypeDefinition seq =
        // method body references
        if m.HasBody = false then [] else
        let instrs = m.Body.Instructions

        let tryFindConversion (i1:Instruction) (i2:Instruction) =
            let i1Code = i1.OpCode.Code
            if (i1Code <> Code.Box && i1Code <> Code.Constrained) || (i2.OpCode.Code <> Code.Callvirt) then None else
            let callMethodRef = i2.Operand :?> MethodReference
            if callMethodRef.Name <> "ToString" || callMethodRef.DeclaringType.FullName <> "System.Object" then None else
            match i1.Operand with
            | :? TypeReference as tr when tr.GetType() = typedefof<TypeReference> || tr.GetType() = typedefof<TypeDefinition> ->
                let objRef = typeResolver tr
                if objRef.IsEnum then Some objRef else None
            | _ -> None

        seq {
            for index = 0 to instrs.Count - 2 do
                let i1 = instrs[index]
                let i2 = instrs[index+1]
                match tryFindConversion i1 i2 with Some td -> yield td | None -> ()
        }

    let private findInModule (typeResolver:TypeResolver) (m:ModuleDefinition) =
        ModuleDefinition.allTypes m
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.collect (fun m -> findInMethod typeResolver m)
        |> Seq.toList


    let findEnumToStringConversions (typeResolver:TypeResolver) (xs:AssemblyDefinition seq) = async {
        let f (x:AssemblyDefinition) = async {
            return x.Modules |> Seq.mapArray (findInModule typeResolver)
        }


        let! interims = xs |> Seq.map f |> Async.Parallel
        return interims |> Array.collect id |> List.concat |> List.distinct |> Array.ofList
    }


let private buildMethodAnalyser (analysis:ReferenceAnalysisInterim) =
    let analyseMethod (m:MethodDefinition) =
        // method body references
        if m.HasBody = false then () else
        let instrs = m.Body.Instructions

        for index = 0 to instrs.Count - 1 do
            let i1 = instrs[index]

            match i1.OpCode.Code with
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
                analysis.MethodReferences.Add(m.Overrides[index]) |> ignore

    analyseMethod


let analyseModule (m:ModuleDefinition) =
    let an = ReferenceAnalysisInterim.create()

    m.GetMemberReferences() |> Seq.iter (ReferenceAnalysisInterim.addMemberReference an)

    let methodAnalyser = buildMethodAnalyser an
    let allTypes = ModuleDefinition.allTypes m |> Seq.toArray
    allTypes |> Seq.collect (fun t -> t.Methods) |> Seq.iter methodAnalyser
    an.TypeReferences.UnionWith(m.GetTypeReferences())
    an


/// One-pass method body analyser.
let analyseAssemblies (rslvr:Resolvers) (assemblyCache:AssemblyCache) (xs:AssemblyDefinition seq) =
    let f (x:AssemblyDefinition) = async {
        return timeThisSeconds "Analysed assembly {Assembly:l}" [| x.Name.Name |] (fun () -> x.Modules |> Seq.mapArray analyseModule)
    }

    let interims = timeThisSeconds "Analysed all assemblies" Array.empty (fun () -> xs |> Seq.map f |> Async.Parallel |> Async.RunSynchronously |> Array.collect id)
    let res = timeThisSeconds "Merged code analysis results" Array.empty (fun () -> ReferenceAnalysisResult.mergeInterim rslvr assemblyCache interims)

    res