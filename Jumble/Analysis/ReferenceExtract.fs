namespace Jumble.Analysis

open System.Collections.Generic
open Mono.Cecil
open Mono.Cecil.Cil
open Jumble
open Jumble.Cecil
open Serilog

module rec ReferenceExtract =
    let rec deriveReference (mr: MemberReference): MemberReference option =
        match mr with
        | :? MethodReference as r -> deriveMethodReference r |> Option.map (fun x -> upcast x)
        | _ -> failwithf "Reference type %s is not supported" (mr.GetType().FullName)

    /// Extracts method reference from mr or its subtypes
    let rec deriveMethodReference (mr: MethodReference): MethodReference option =
        match mr with
        | :? MethodDefinition -> None
        | :? GenericInstanceMethod as gim -> deriveMethodReference gim.ElementMethod
        | _ when mr.GetType() = typeof<MethodReference> -> Some mr
        | _ -> failwithf "%A is not supported" mr

    let private extractFromInstruction (i: Instruction) =
        let notsup () = failwithf "Instruction code %O has unsupported operand %O" i.OpCode i.Operand

        match i.OpCode.Code with
        | Code.Call
        | Code.Callvirt ->
            match i.Operand with
            | :? GenericInstanceMethod as gim -> deriveReference gim.ElementMethod
            | :? MethodReference as mr -> deriveReference mr
            | _ -> notsup ()
        | Code.Ldfld
        | Code.Ldflda
        | Code.Ldsfld
        | Code.Ldsflda
        | Code.Stfld
        | Code.Stsfld ->
            match i.Operand with
            | :? FieldReference as r -> Some(upcast r)
            | _ -> notsup ()
        | Code.Ldftn
        | Code.Ldvirtftn ->
            match i.Operand with
            | :? MethodDefinition -> None
            | :? MethodReference as mr -> deriveReference mr
            | _ -> notsup ()
        | _ -> None

    let private extractFromMethod (m: MethodDefinition) =
        if m.Body = null then
            Seq.empty
        else
            let bodyMethodRefs = m.Body.Instructions |> Seq.choose extractFromInstruction

            let overrideMethodRefs =
                if m.Overrides = null then Seq.empty else m.Overrides |> Seq.cast<MemberReference>
            Seq.concat [ bodyMethodRefs; overrideMethodRefs ]

    let private extractFromType (t: TypeDefinition) =
        t.Methods |> Seq.collect extractFromMethod

    /// Finds all method references in a module. Includes also refs contained in CIL
    let getAllMemberReferences (md: ModuleDefinition) =
        let refs =
            ModuleDefinition.allTypes md
            |> Seq.collect extractFromType
            |> Seq.append (md.GetMemberReferences())
            |> Seq.distinct
            |> Seq.toArray

        refs

    let getAllTypeReferences (md: ModuleDefinition) =
        let refs = md.GetTypeReferences() |> Seq.toArray
        refs

    /// Finds whatever in the assembly and all assemblies that reference it
    let findInModulesRecursive<'T> (find: ModuleDefinition -> 'T []) (atn: AssemblyTreeNode): 'T seq =
        // we also have to rename references in the same module(!)
        seq {
            yield atn
            yield! atn.ReferencedByRec
        }
        |> Seq.collect (fun mtn -> mtn.Assembly.Modules |> Seq.collect find)

    let findTypeReferences (resolveRef: TypeReference -> TypeDefinition) (refsInModule: ModuleDefinition -> TypeReference []) (t: TypeTreeNode) =
        findInModulesRecursive refsInModule t.AssemblyTreeNode
        |> Seq.filter (fun tr -> resolveRef tr = t.TypeDefinition)
        |> Seq.toList

    /// Builds a function that returns all references for given member definition
    // Usually this serves as a catalogue of members that should be also renamed
    let buildMemberRefFinder (resolveRef: MemberDefResolver) (refs: MemberReference seq): MemberRefFinder =
        let cache = Dictionary<IMemberDefinition, IList<MemberReference>>()

        let addReference (r: MemberReference) =
            match resolveRef r with
            // multi-dimensional array methods, e.g. Get(), Set(), .ctor. There is no 'declaring member'.
            | null when r.DeclaringType.IsArray -> ()

            | null -> Log.Warning("Reference {Ref} in module {Module} cannot be resolved", r.FullName, r.Module.Name)

            | def ->
                match cache.TryGetValue def with
                | true, list -> list.Add(r)
                | false, _ ->
                    let list = List<MemberReference>()
                    list.Add(r)
                    cache.Add(def, list)

        refs |> Seq.iter addReference

        fun (def: IMemberDefinition) ->
            match cache.TryGetValue def with
            | true, list -> list :> MemberReference seq
            | false, _ -> Seq.empty