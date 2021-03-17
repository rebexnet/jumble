namespace Jumble.Export

open FSharpPlus

[<AutoOpen>]
module Types = 
    open Mono.Cecil
    open Jumble
    open Jumble.Analysis
    open Jumble.Rename

    type ExportFilterStrategy = 
    /// Copies only modified dlls
    | ModifiedOnly

    type ExportTargetStrategy = 
    /// Do not save anything
    | DryRun
    /// Disregards original directory structure and puts all files in same directory
    | FlattenTo of string
    
    type OutputOptions = {
        ExportFilter: ExportFilterStrategy
        ExportTarget: ExportTargetStrategy
    }
    
    type TypeMap = {
        Type: TypeType
        OriginalName: TypeDefinitionName
        NewName: TypeDefinitionName
        Members: MemberMap[]
    }

    module rec TypeWithMembersRenameResult = 
        let fromTypeRenamePlan (memberPlans:MemberRenamePlan[]) (typePlan:TypeRenamePlan) = 
            let template = fromTypeUnchanged memberPlans typePlan.Type
            { template with TypeMap.NewName = typePlan.NewName }

        let fromTypeUnchanged (memberPlans:MemberRenamePlan[]) (t:TypeDefinition) = 
            let name = TypeDefinitionName.fromTypeDefinition t
            { Type = TypeType.fromTypeDefinition t
              OriginalName = name
              NewName = name
              Members = memberPlans |> Array.map MemberMap.fromMemberRenamePlan }

    type AssemblyMap = {
        Assembly: string
        Types: TypeMap[]
    }

    type RenameMap = {
        Assemblies: AssemblyMap[]
    }

    type ExportedModulePaths = {
        OriginalPath: string
        ExportedPath: string
    }

    type ExportResult = {
        Dlls: ExportedModulePaths list
    }

    module RenameMap = 
        let fromRenameResult (r:RenameResult) : RenameMap =
            // all types being renamed plus all types whose members are being renamed
            let allTypes = r.TypeRenamePlans
                           |> Seq.map (fun trp -> trp.Type)
                           |> Seq.append (r.MemberRenamePlans |> Seq.map (fun mr -> mr.Member.DeclaringType))
                           |> Seq.distinct
                           |> Seq.toArray

            let typeLookup = r.TypeRenamePlans |> Array.map (fun p -> p.Type, p) |> dict
            let memberLookup = r.MemberRenamePlans |> Array.groupBy (fun p -> p.Member.DeclaringType) |> dict

            let ungrouped = allTypes 
                            |> Array.map (fun t ->
                                (t,
                                 Dict.tryGetValue t typeLookup,
                                 Dict.tryGetValue t memberLookup |> Option.defaultValue Array.empty))
                            |> Array.map (fun (t, trpOpt, mrs) ->
                                (t, match trpOpt with 
                                    | None -> TypeWithMembersRenameResult.fromTypeUnchanged mrs t 
                                    | Some trp -> TypeWithMembersRenameResult.fromTypeRenamePlan mrs trp))

            let assemblyMaps = ungrouped 
                               |> Seq.groupBy (fun (t, _) -> t.Module.Assembly)
                               |> Seq.map (fun (a, typesAndTmrs) -> (a, typesAndTmrs |> Seq.map snd |> Seq.toArray))
                               |> Seq.map (fun (a, tmrs) -> { AssemblyMap.Assembly = a.FullName; Types = tmrs })
                               |> Seq.toArray

            { Assemblies = assemblyMaps }