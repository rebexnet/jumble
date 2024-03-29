﻿namespace Jumble.Analysis

open Jumble
open Mono.Cecil
open Serilog
open System.Collections.Generic
open Serilog.Events

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
                match group |> Seq.tryFind(fun x -> x.Member = gr.Member) with
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
                | true, g when g = finalGroup -> 
                    addOrReplaceInGroup finalGroup m
                    
                | true, g when g <> finalGroup -> 
                    // move all from g to group
                    for gres in g do 
                        addOrReplaceInGroup finalGroup gres
                    addOrReplaceInGroup finalGroup m

                    g |> Seq.iter (fun gm -> lookup[gm.Member] <- finalGroup)
                | _ -> 
                    addOrReplaceInGroup finalGroup m
                    lookup.Add(m.Member, finalGroup)

        member _.Add (ms:GroupingResult seq) = 
            // one has to consider that there might be disjunct groups that we need to union
            let group = ms 
                        |> Seq.tryPick(fun m -> match lookup.TryGetValue(m.Member) with true, g -> Some g | _ -> None)
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
            | _ when t.IsClass ->
                match m with 
                | :? MethodDefinition as md ->
                    // IsAbstract is needed as F# does not add "newslot" to abstract methods
                    if md.IsVirtual && (md.IsNewSlot || md.IsAbstract) then
                        yield! MethodLookupFunctions.findVirtualMethodOverrides md t
                               |> List.map (fun me -> mkResult me (OverrideMethod md))
                               
                | _ -> ()
                
            // when m is defined on an interface
            | _ when t.IsInterface ->
                match m with
                | :? MethodDefinition as md when md.IsStatic = false ->
                    let methodImplementations = t.Children
                                                |> Seq.collect (fun ttn -> InterfaceMethodImplSearch.findInterfaceMethodImplementations md ttn.TypeDefinition)

                    yield! methodImplementations
                           // explicit implementations can have any name so we won't group them
                           |> Seq.filter (MethodDefinition.isProbablyInterfaceExplicitImplementation >> not)
                           |> Seq.map (fun methodDef -> mkResult methodDef (InterfaceImplementation md))
                | _ -> ()
                
            | _ -> failwithf "Type class is not supported"
        ] 


    /// Groups all type members into groups whose members should have same name
    let groupMembers (types: TypeTree) : GroupingResult[][] =
        Log.Information("Searching for type members in {Types} types...", types.AllTypes |> Seq.length)
        
        let allMembersArray = types.AllTypes
                            |> Seq.collect _.MemberDefinitions
                            |> Seq.distinct
                            |> Seq.toArray

        Log.Information("Grouping {Members} type members...", allMembersArray.Length)

        let memberGroups = MemberGroups()
        let isVerbose = Log.Logger.IsEnabled(LogEventLevel.Verbose)

        for m in allMembersArray do
            let members = findAssociatedMembers types (m :> obj :?> IMemberDefinition)
            memberGroups.Add members |> ignore
            if isVerbose then
                Log.Verbose("Found associated members for {Member}: {Members}", m.FullName, members |> Seq.map _.Member.FullName |> String.concat ", ")
        
        let groups = memberGroups.GetGroups()
        Log.Debug("Found {groups} groups", groups.Length)
        groups

    let splitGroups (groups:GroupingResult[][]) : IMemberDefinition[][] =
        groups |> Array.map (fun g -> g |> Array.map _.Member)