namespace Jumble.Rename

open FSharpPlus
open Jumble.Analysis
open Jumble.Rename.Exclusion
open Jumble.Utils
open Mono.Cecil
open Serilog

module RenameFilter =
    type FilterResult = {
        FilteredGroups: GroupingResult[][]
        Exclusions: Exclusions
    }
    
    let private dllOoByMemberBuilder (asms:AssemblyObfuscationOptions list) = 
        let asmsLookup = asms |> List.map (fun asm -> (asm.Assembly.Name.Name, asm.Options)) |> dict
        let lookup (m:IMemberDefinition) : DllObfuscationOptions =
            match asmsLookup.TryGetValue(m.DeclaringType.Module.Assembly.Name.Name) with 
            | false, _ -> failwithf "Assembly %s cannot be found" m.DeclaringType.Module.Assembly.Name.Name
            | true, res -> res
        lookup
    
    let private hasGroupAnyMemberSubjectToRenaming (lookup:IMemberDefinition -> DllObfuscationOptions) (gs:GroupingResult[]) =
        gs |> Array.exists (fun g -> let lvl = (lookup g.Member).ObfuscationLevel in lvl = PrivateAndPublic || lvl = PrivateOnly)
    
    let private filterGroup (excl:Exclusions) (gs:GroupingResult[]) =
        match gs |> Array.tryPick (fun g -> excl.filterMember g.Member) with
        | None -> Keep
        | Some _ ->
            gs
            |> Seq.map (fun g -> ExclusionScopeAndReason.createMember g.Member GroupExcluded)
            |> Seq.iter excl.add
            Exclude
    
    /// Keeps only groups whose all members belong to allowed modules list
    let filterGroups (asms:AssemblyObfuscationOptions list) (exclusions:Exclusions) (groups: GroupingResult[][]) : GroupingResult[][] =
        Log.Information("Prefiltering groups...")
        let dllOoByMember = dllOoByMemberBuilder asms

        // keep only groups which have potential rename candidates 
        let preFiltered = groups |> Array.filter (hasGroupAnyMemberSubjectToRenaming dllOoByMember)
        Log.Debug("Prefiltered down to {group} groups", preFiltered.Length)
        
        Log.Information("Filtering groups...")
        let filtered = preFiltered |> Array.filter (fun gs -> filterGroup exclusions gs = Keep)
        Log.Debug("Filtered down to {groups} groups", filtered.Length)
        
        filtered
    
    let filterTypes (asmObfLevel:TypeDefinition -> ObfuscationLevel) (exclusions:Exclusions) (ts:TypeTreeNode seq) : TypeTreeNode[] =
        let flt (t:TypeDefinition) =
            match asmObfLevel t with
            | OnlyNecessary | Untouchable -> false
            | _ when exclusions.filterType t = None -> true
            | _ -> false
        
        Log.Information("Filtering types...")    
        let result = ts |> Seq.filter (fun t -> flt t.TypeDefinition) |> Seq.toArray
        Log.Debug("Filtered down to {Types} types", result.Length)
        result