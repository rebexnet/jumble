module Jumble.Rename.Exclusion.WhitelistFilter

open Jumble
open Jumble.Rename.Types

/// Creates a whitelist filter from the list of member / type names to be excluded
let createWhitelistFilters (idents:IdentifierSpec list) : ExclusionFilterType list =
    let matchName (n:string) =
        idents |> Seq.exists (fun ident -> IdentifierSpec.matches ident n)
    let typeLevelFilter (tc:TypeFilterContext) =
        if matchName tc.Type.TypeDefinition.FullName then
            ExclusionScopeAndReason.createType tc.Type.TypeDefinition AppliesToAllMembers Whitelisted |> Seq.singleton
        else
            Seq.empty
            
    let memberLevelFilter (m:MemberFilterContext) =
        if matchName $"%s{m.Member.DeclaringType.FullName}.%s{m.Member.Name}" then
            ExclusionScopeAndReason.createMember m.Member Whitelisted |> Seq.singleton
        else
            Seq.empty
            
    [
        TypeLevelFilter typeLevelFilter
        MemberLevelFilter memberLevelFilter
    ]