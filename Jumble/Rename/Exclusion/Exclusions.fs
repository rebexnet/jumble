namespace Jumble.Rename.Exclusion

open FSharpPlus
open Mono.Cecil
open System.Collections.Generic

open Jumble.Rename
open Serilog

type Exclusions() =
    let assemblies = Dictionary<AssemblyDefinition, ExclusionReason list>()
    let typesWithMembers = Dictionary<TypeDefinition, ExclusionReason list>()
    let typeNamesOnly = Dictionary<TypeDefinition, ExclusionReason list>()
    let members = Dictionary<IMemberDefinition, ExclusionReason list>()
    
    member this.filterAssembly (a:AssemblyDefinition) =
        Dict.tryGetValue a assemblies
    
    member this.filterType (t:TypeDefinition) =
        this.filterAssembly t.Module.Assembly
        |> Option.orElseWith (fun () -> Dict.tryGetValue t typesWithMembers)
        |> Option.orElseWith (fun () -> Dict.tryGetValue t typeNamesOnly)
    
    member this.filterMember (m:IMemberDefinition) =
        this.filterAssembly m.DeclaringType.Module.Assembly
        |> Option.orElseWith (fun () -> Dict.tryGetValue m.DeclaringType typesWithMembers)
        |> Option.orElseWith (fun () -> Dict.tryGetValue m members)
        
    member this.add (m:ExclusionScopeAndReason) =
        let (scope, reason) = m
        let inline append (d:Dictionary<_, ExclusionReason list>) v =
            match d.TryGetValue v with
            | (false, _) -> d.Add(v, [reason])
            | (true, l) -> d.[v] <- reason::l 
            
        match scope with
        | AssemblyScope a -> append assemblies a
        | MemberScope m -> append members m
        | TypeScope t when t.IncludeMembers = AppliesToAllMembers -> append typesWithMembers t.Type
        | TypeScope t when t.IncludeMembers = AppliesToTypeNameOnly -> append typeNamesOnly t.Type
        | _ -> failwith "not supported"
    
    member this.siftEnumToString () =
        // warning: tohle je jen docasne pro Pavla
        let keysToRemove = typesWithMembers
                           |> Seq.filter (fun kvp -> kvp.Value |> List.forall (fun r -> match r with StringConversion -> true | _ -> false))
                           |> Seq.map (fun kvp -> kvp.Key)
                           |> Seq.toList
        
        keysToRemove
        |> List.iter (fun t ->
            // this is dodgy
            if (not <| assemblies.ContainsKey(t.Module.Assembly)) then
                Log.Warning("Type {Type} conversion to string detected", t.FullName)
            typesWithMembers.Remove(t) |> ignore
        )
     
    static member create (ms:ExclusionScopeAndReason seq) =
        let excl = Exclusions()
        ms |> Seq.iter excl.add
        excl
        
        
        
type ExclusionFilter = IMemberDefinition -> ExclusionFilterResult 

module ExclusionFilter =
    let keepAll : ExclusionFilter = fun _ -> Keep 
