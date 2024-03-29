﻿namespace Jumble.Rename

open System
open Jumble.Analysis
open Jumble.Analysis.CodeAnalysis

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

    type TypeRenamePlan = {
        TypeID: MemberID
        NewName: TypeDefinitionName

        // keeping original name here as it's used by mapfile exporter
        OriginalName: TypeDefinitionName
    }

    type MemberRenamePlan = {
        MemberID: MemberID
        NewName: string
        NewParameters: string list

        // keeping original name here as it's used by mapfile exporter
        OriginalName: string
        OriginalParameters: string list
    }

    type ModuleRenamePlan = {
        TypeRenamePlans: Map<uint32, TypeRenamePlan>
        MemberRenamePlans: Map<uint32, MemberRenamePlan>
        MVID: MVID
    }

    type RenamePlans<'T> = private {
        cache: Map<MVID, Map<uint32, 'T>>
    }
    with
        member this.tryFind (memberId:MemberID) : 'T option =
            this.cache
            |> Map.tryFind memberId.MVID
            |> Option.bind (fun bt -> bt |> Map.tryFind (memberId.MemberToken.ToUInt32()))

        member this.byModule (m:MVID) =
            this.cache
            |> Map.tryFind m
            |> Option.defaultValue Map.empty

        static member create<'T> (idf: 'T -> MemberID) (plans: 'T[]) =
            let cache =
                plans
                |> Array.groupBy (fun p -> (idf p).MVID)
                |> Array.map (fun (mvid, ps) -> (mvid, (ps |> Array.map (fun p -> (idf p).MemberToken.ToUInt32(), p)) |> Map.ofArray))
                |> Map.ofArray

            { cache = cache }

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
            MemberLevelFilter (fun m -> f m |> Option.map (ExclusionScopeAndReason.createMember m.Member) |> Option.toSeq)
        
        let fromOptionTypeFilter inclMembers f : ExclusionFilterType =
            TypeLevelFilter (fun t -> f t |> Option.map (fun r -> ExclusionScopeAndReason.createType t.Type.TypeDefinition inclMembers r) |> Option.toSeq)
        
    type ExclusionFilterResult = Exclude | Keep