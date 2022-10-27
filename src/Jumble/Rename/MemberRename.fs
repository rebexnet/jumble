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
    
    let private renameMember (findRefs:MemberReferenceLookup) (mmbr:IMemberDefinition) (name:string, parameters: string list): unit =
        let memberRefs = findRefs mmbr
        let newName = fromCanonicalName name mmbr
        
        memberRefs |> Seq.iter (fun (r:MemberReference) -> r.Name <- newName)
        Log.Verbose("Renaming member {Orig} to {New}", mmbr.FullName, name)
        mmbr.Name <- newName
        (MemberDefinition.parameters mmbr, parameters)
        ||> Seq.iter2 (fun pd newName -> if pd.Name <> newName then pd.Name <- newName)
        
    let private createGroupRenamePlan memberNameGen (parameterNameGen:ParameterNameGenerator) (group:MemberGroup) =
        let newCanonicalName = memberNameGen group
        
        group |> Seq.map (fun m ->
            let parameters = MemberDefinition.parameters m |> Seq.map (fun p -> p.Name) |> Seq.toList
            let newParameters = if m :? MethodDefinition then MemberDefinition.parameters m |> Seq.map (fun p -> parameterNameGen p) |> Seq.toList else parameters
            
            { MemberRenamePlan.MemberID = MemberID.fromDefinition m
              NewName = newCanonicalName
              NewParameters = newParameters
            })

    let createRenamePlans (memberNameGen:MethodNameGenerator) (parameterNameGen:ParameterNameGenerator) (memberGroups: MemberGroup[]) : MemberRenamePlan[] =
        memberGroups |> Seq.collect (createGroupRenamePlan memberNameGen parameterNameGen) |> Seq.toArray
        
    let renameMembers (memberRefFinder:MemberReferenceLookup) (memberIDLookup:MemberIDLookup) (plans: MemberRenamePlan[]) : unit =
        Log.Information("Renaming {Members} members....", plans.Length)
        for plan in plans do
            renameMember memberRefFinder (memberIDLookup plan.MemberID) (plan.NewName, plan.NewParameters)