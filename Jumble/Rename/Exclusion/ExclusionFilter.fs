namespace Jumble.Rename

open Jumble
open Jumble.Analysis
open Jumble.Rename.Types
open Jumble.Rename.Types.ExclusionFilterType
open Jumble.Cecil
open Jumble.Rename.Exclusion
open Mono.Cecil

module ExclusionFilter =
    /// Filters out assemblies marked as Untouchable
    let private fltObfLevel (a:AssemblyFilterContext) =
        match a.ObfuscationLevel with
        | ObfuscationLevel.Untouchable ->
            Seq.singleton (ExclusionScopeAndReason.createAsm a.Assembly Whitelisted)
        | _ -> Seq.empty
    
    let buildFilters (memberRes:MemberDefResolver, typeRes:TypeNodeResolver) =
        let filters = [ 
            AssemblyLevelFilter fltObfLevel
            AssemblyLevelFilter (fun a -> CustomAttributeExtract.fltCustomAttributeCtorVals memberRes a.Assembly |> Seq.ofList)
            fromOptionTypeFilter AppliesToTypeNameOnly TypeFilters.fltTypeByVisibility 
            fromOptionTypeFilter AppliesToAllMembers TypeFilters.fltSpecialNames
            TypeLevelFilter (fun t -> CustomAttributeExtract.fltCustomAttributeCtorVals memberRes t.Type.TypeDefinition |> Seq.ofList)
            TypeLevelFilter (fun t -> ObfuscationAttributeFilter.fltObfuscationAttributeOnType typeRes t)
            fromOptionMemberFilter MemberFilters.fltPublicMember
            fromOptionMemberFilter MemberFilters.fltIndexer
            fromOptionMemberFilter MemberFilters.fltOperator
            fromOptionMemberFilter MemberFilters.fltRuntimeSpecialName
            fromOptionMemberFilter MemberFilters.fltDllImport
            fromOptionMemberFilter MemberFilters.fltDelegate
            MemberLevelFilter (fun m -> CustomAttributeExtract.fltCustomAttributeCtorVals memberRes m.Member |> Seq.ofList)
            MemberLevelFilter (ScanEnumToString.fltEnumToString memberRes)
        ]
        
        filters
    
    let private getExclusions (ttnResolver: TypeNodeResolver) filters (asm:AssemblyFilterContext) =
        
        let asmLevelFilters = filters |> List.choose (fun f -> match f with AssemblyLevelFilter af -> Some af | _ -> None)
        let typeLevelFilters = filters |> List.choose (fun f -> match f with TypeLevelFilter tf -> Some tf | _ -> None)
        let memberLevelFilters = filters |> List.choose (fun f -> match f with MemberLevelFilter mf -> Some mf | _ -> None)

        seq {
            yield! asmLevelFilters |> Seq.collect (fun f -> f asm)
        
            for t in AssemblyDefinition.allTypes asm.Assembly do
                let ttn = ttnResolver t
                
                let typeContext = TypeFilterContext.create ttn asm.ObfuscationLevel
                yield! typeLevelFilters |> Seq.collect (fun f -> f typeContext)
                
                for m in TypeDefinition.members t do
                    let memberContext = MemberFilterContext.create m asm.ObfuscationLevel
                    yield! memberLevelFilters |> Seq.collect (fun f -> f memberContext)
        }
        
    let findExclusions (ttnResolver: TypeNodeResolver) filters (asm:AssemblyObfuscationOptions) =
        let asmContext = AssemblyFilterContext.create asm.Assembly asm.Options.ObfuscationLevel
        getExclusions ttnResolver filters asmContext