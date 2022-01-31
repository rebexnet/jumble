namespace Jumble.Rename.Exclusion

open Jumble
open Jumble.Rename

module TypeFilters =
    let fltTypeByVisibility (t:TypeFilterContext) =
        match t.AssemblyObfuscationLevel with
        | PrivateAndPublic -> None
        | PrivateOnly when TypeDefinition.isPublicVisible t.Type.TypeDefinition = false -> None
        | _ when TypeDefinition.isPublicVisible t.Type.TypeDefinition -> Some PublicMember
        | _ -> None
    
    let fltSpecialNames (t:TypeFilterContext) =
        if t.Type.TypeDefinition.Name = "<Module>" then Some RuntimeSpecialName else None