namespace Jumble.Rename

[<AutoOpen>]
module rec Types = 
    open Mono.Cecil
    
    open Jumble
    
    type ExclusionFilterName =
    | FltEnumToString
    
    module ExclusionFilterName =
        let parse (s:string) =
            match s.ToLowerInvariant() with
            | "fltenumtostring" -> FltEnumToString
            | _ -> failwithf $"Unsupported exclusion filter name: %s{s}"
    
    type MemberGroup = IMemberDefinition[]

    type ObfuscationLevel =
        /// No renaming will be done
        | Untouchable
        /// Only types deriving from rename candidates in referenced assemblies will be renamed 
        | OnlyNecessary
        /// Only non-visible types will be renamed
        | PrivateOnly
        /// All types will be renamed
        | PrivateAndPublic
    
    type MemberInclusion =
    /// Applies to public and private members
    | AppliesToAllMembers
    /// Applies to type name only
    | AppliesToTypeNameOnly
    
    type TypeScope = {
        Type: TypeDefinition
        IncludeMembers: MemberInclusion
    }
    
    type Scope =
    | AssemblyScope of AssemblyDefinition
    | TypeScope of TypeScope
    | MemberScope of IMemberDefinition
    
    type DllObfuscationOptions = {
        DllPath: string
        ObfuscationLevel: ObfuscationLevel
        ExceptFilters: ExclusionFilterName list
        SigningKey: SigningKey option
    }
    with
        member this.Modifiable  
            with get () = 
                match this.ObfuscationLevel with 
                | PrivateAndPublic | PrivateOnly | OnlyNecessary -> true
                | ObfuscationLevel.Untouchable -> false
    
    module DllObfuscationOptions =
        let fromDllPath obfLevel path exceptFilters = {
                                          DllObfuscationOptions.DllPath = path;
                                          ObfuscationLevel = obfLevel
                                          ExceptFilters = exceptFilters
                                          SigningKey = None
                                        }

        let fromDllPathWithKey obfLevel path exceptFilters key = { fromDllPath obfLevel path exceptFilters with SigningKey = Some key }
        
    type AssemblyObfuscationOptions = {
        Assembly: AssemblyDefinition
        Options: DllObfuscationOptions
    }

    type MemberMap = {
        OriginalSignature: MemberSignature
        NewSignature: MemberSignature
    }

    module MemberMap = 
        let fromMemberRenamePlan (m:MemberRenamePlan) = 
            let original = MemberSignature.create m.Member
            let n = match original with 
                    | MethodS s -> MethodS { s with Name = m.NewName }
                    | EventS s -> EventS { s with Name = m.NewName }
                    | PropertyS s -> PropertyS { s with Name = m.NewName }
                    | FieldS s -> FieldS { s with Name = m.NewName }

            { MemberMap.OriginalSignature = original; NewSignature = n }
        
    type TypeRenamePlan = {
        Type: TypeDefinition
        OriginalName: TypeDefinitionName
        NewName: TypeDefinitionName
    }

    type MemberRenamePlan = {
        Member: IMemberDefinition
        OriginalName: string
        NewName: string
        OriginalParameters: string list
        NewParameters: string list
    }

    type RenameResult = {
        TypeRenamePlans: TypeRenamePlan[]
        MemberRenamePlans: MemberRenamePlan[]
    }
        
    type AssemblyFilterContext = {
         Assembly: AssemblyDefinition
         ObfuscationLevel: ObfuscationLevel
    }
    with static member create a lvl = { Assembly = a; ObfuscationLevel = lvl }

    type TypeFilterContext = {
        Type: TypeTreeNode
        AssemblyObfuscationLevel: ObfuscationLevel
    }
    with static member create t lvl = { Type = t; AssemblyObfuscationLevel = lvl }

    type MemberFilterContext = {
        Member: IMemberDefinition
        AssemblyObfuscationLevel: ObfuscationLevel
    }
    with static member create m lvl = { Member = m; AssemblyObfuscationLevel = lvl }

    type ExclusionReason =
        | PublicMember
        | StringConversion
        | AssemblyExcluded
        | Delegate
        | DllImport
        | Indexer
        | Operator
        | RuntimeSpecialName
        | CustomAttributeValue
        | GroupExcluded
        | Whitelisted
        | CecilNotSupported of string

    type ExclusionScopeAndReason = Scope * ExclusionReason

    module ExclusionScopeAndReason =
        let create m r : ExclusionScopeAndReason = (m, r)
        let createAsm a r = create (AssemblyScope a) r
        let createType t incl r = create (TypeScope { Type = t; IncludeMembers = incl }) r
        let createMember m r = create (MemberScope m) r

    type ExclusionFilterType =
    | AssemblyLevelFilter of (AssemblyFilterContext -> ExclusionScopeAndReason seq)  
    | TypeLevelFilter of (TypeFilterContext -> ExclusionScopeAndReason seq)
    | MemberLevelFilter of (MemberFilterContext -> ExclusionScopeAndReason seq)

    module ExclusionFilterType =
        let fromOptionMemberFilter f : ExclusionFilterType =
            MemberLevelFilter (fun m -> f m |> Option.map (fun r -> ExclusionScopeAndReason.createMember m.Member r) |> Option.toSeq)
        
        let fromOptionTypeFilter inclMembers f : ExclusionFilterType =
            TypeLevelFilter (fun t -> f t |> Option.map (fun r -> ExclusionScopeAndReason.createType t.Type.TypeDefinition inclMembers r) |> Option.toSeq)
        
    type ExclusionFilterResult = Exclude | Keep