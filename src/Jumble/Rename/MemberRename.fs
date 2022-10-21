namespace Jumble.Rename

open Jumble
open Jumble.Analysis
open Jumble.Rename.NameGenerators
open Mono.Cecil
open Serilog

module MemberRename =
    let private fromCanonicalName canonicalName (m:IMemberDefinition) =
        match m with 
        | :? MethodDefinition as m when m.IsSetter -> $"set_%s{canonicalName}"
        | :? MethodDefinition as m when m.IsGetter -> $"get_%s{canonicalName}"
        | _ -> canonicalName
    
    let private renameMember (findRefs:MemberLookup) (m:MemberRenamePlan) : unit =
        let memberRefs = findRefs m.Member
        let newName = fromCanonicalName m.NewName m.Member
        
        memberRefs |> Seq.iter (fun (r:MemberReference) -> r.Name <- newName)
        Log.Verbose("Renaming member {Orig} to {New}", m.Member.FullName, m.NewName)
        m.Member.Name <- newName
        (MemberDefinition.parameters m.Member, m.NewParameters)
        ||> Seq.iter2 (fun pd newName -> if pd.Name <> newName then pd.Name <- newName)
        
    let private createGroupRenamePlan memberNameGen (parameterNameGen:ParameterNameGenerator) (group:MemberGroup) =
        let newCanonicalName = memberNameGen group
        
        group |> Seq.map (fun m ->
            let parameters = MemberDefinition.parameters m |> Seq.map (fun p -> p.Name) |> Seq.toList
            let newParameters = if m :? MethodDefinition then MemberDefinition.parameters m |> Seq.map (fun p -> parameterNameGen p) |> Seq.toList else parameters
            
            { MemberRenamePlan.Member = m
              OriginalName = m.Name
              NewName = newCanonicalName
              OriginalParameters = parameters
              NewParameters = newParameters
            })

    let createRenamePlans (memberNameGen:MethodNameGenerator) (parameterNameGen:ParameterNameGenerator) (memberGroups: MemberGroup[]) : MemberRenamePlan[] =
        memberGroups |> Seq.collect (createGroupRenamePlan memberNameGen parameterNameGen) |> Seq.toArray
        
    let renameMembers (memberRefFinder:MemberLookup) (plans: MemberRenamePlan[]) : unit =
        Log.Information("Renaming {Members} members....", plans.Length)
        plans |> Array.iter(renameMember memberRefFinder)