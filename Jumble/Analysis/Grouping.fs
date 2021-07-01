namespace Jumble.Analysis

open FSharp.Core.Fluent
open Jumble
open Mono.Cecil
open Serilog
open System.Collections.Generic

[<AutoOpen>]
module Grouping = 

    type GroupingReason = 
        | NoReason
        | InterfaceImplementation of IMemberDefinition
        | OverrideMethod of MethodDefinition
        | PropertyAccessorMethod of PropertyDefinition
    
    type GroupingResult = {
        Member: IMemberDefinition
        Reason: GroupingReason
    }
    
    let private mkResult m reason = { Member = m; Reason = reason }

    type MemberGroups () = 
        let lookup = Dictionary<IMemberDefinition, ResizeArray<GroupingResult>>()
        
        let rec assignGroup finalGroup (ms:GroupingResult seq) = 
            let addOrReplaceInGroup (group:ResizeArray<GroupingResult>) (gr:GroupingResult) = 
                match group.tryFind(fun x -> x.Member = gr.Member) with 
                | None -> group.Add(gr)
                | Some existing -> 
                    match existing.Reason, gr.Reason with 
                    | _, NoReason -> ()
                    | NoReason, _ -> 
                        group.Remove(existing) |> ignore
                        group.Add(gr)
                    | _ -> () 

            for m in ms do 
                match lookup.TryGetValue(m.Member) with
                | (true, g) when g = finalGroup -> 
                    addOrReplaceInGroup finalGroup m
                    
                | (true, g) when g <> finalGroup -> 
                    // move all from g to group
                    for gres in g do 
                        addOrReplaceInGroup finalGroup gres
                    addOrReplaceInGroup finalGroup m

                    g |> Seq.iter (fun gm -> lookup.[gm.Member] <- finalGroup)
                | _ -> 
                    addOrReplaceInGroup finalGroup m
                    lookup.Add(m.Member, finalGroup)

        member _.Add (ms:GroupingResult seq) = 
            // one has to consider that there might be disjunct groups that we need to union
            let group = ms 
                        |> Seq.tryPick(fun m -> match lookup.TryGetValue(m.Member) with (true, g) -> Some g | _ -> None)
                        |> Option.defaultWith (fun () -> ResizeArray<GroupingResult>())
            
            assignGroup group ms
            group

        member _.GetGroups() = lookup.Values |> Seq.distinct |> Seq.map(fun g -> g |> Seq.toArray) |> Seq.toArray

    let private findAssociatedMembers (types:TypeTree) (m:IMemberDefinition) : GroupingResult list =
        [
            let getNode = types.GetNode

            yield mkResult m NoReason

            let t = getNode m.DeclaringType
            
            // include getter and setter for a property
            // todo: is this necessary?
            match m with 
            | :? PropertyDefinition as p ->
                if p.SetMethod <> null then yield mkResult p.SetMethod (PropertyAccessorMethod p)
                if p.GetMethod <> null then yield mkResult p.GetMethod (PropertyAccessorMethod p)
            | _ -> ()

            match t with 
            // when m is defined on a class/struct
            | _ when t.TypeDefinition.IsClass -> 
                match m with 
                | :? MethodDefinition as md ->
                    if md.IsVirtual && md.IsNewSlot then
                        yield! MethodLookupFunctions.findVirtualMethodOverrides md t
                               |> List.map (fun me -> mkResult me (OverrideMethod md))
                               
                | _ -> ()
                
            // when m is defnied on an interface
            | _ when t.TypeDefinition.IsInterface ->
                match m with
                | :? MethodDefinition as md when md.IsStatic = false ->
                    // all descentants should implement the given method - either directly or via an ancestor

                    // find all first descentands in the inheritance chain which are not interfaces
                    let rec firstNonInterfaceDescendants (t:TypeTreeNode) =
                        if t.TypeDefinition.IsInterface then
                            Seq.collect firstNonInterfaceDescendants t.Children
                        else
                            Seq.singleton t

                    let nonInterfaceDescendants = firstNonInterfaceDescendants t
                    yield! nonInterfaceDescendants
                           |> Seq.collect (fun d -> findInterfaceMethodImplementationAcrossMultipleInheritance d.TypeDefinition md)
                           // filter out iface explicit implementations - they can have any name
                           |> Seq.filter (fun m -> (m.IsPrivate && m.IsHideBySig) = false)
                           |> Seq.map (fun methodDef -> mkResult methodDef (InterfaceImplementation md))
                | _ -> ()
                
            | _ -> failwithf "Type class is not supported"
        ] 


    /// Groups all type members into groups whose members should have same name
    let groupMembers (types: TypeTree) : GroupingResult[][] =
        Log.Information("Searching for type members in {Types} types...", types.AllTypes.length)
        
        let allMembersSeq = types.AllTypes
                            |> Seq.collect (fun t -> TypeDefinition.members t.TypeDefinition)
                            |> Seq.distinct
                            |> Seq.toArray
        
        Log.Information("Grouping {Members} type members...", allMembersSeq.length)
        
        let memberGroups = MemberGroups()
        for m in allMembersSeq do
            let members = findAssociatedMembers types m
            memberGroups.Add members |> ignore
        
        let groups = memberGroups.GetGroups()
        Log.Debug("Found {groups} groups", groups.Length)
        groups

    let splitGroups (groups:GroupingResult[][]) : IMemberDefinition[][] =
        groups |> Array.map (fun g -> g |> Array.map(fun g -> g.Member))