namespace Jumble.Rename.Exclusion

open Mono.Cecil
open Jumble
open Jumble.Rename
open Jumble.Utils

module MemberFilters =
    // why do we even have this?
    let fltDelegate (m:MemberFilterContext) =
        Option.fromBool ExclusionReason.Delegate (TypeDefinition.isDelegate m.Member.DeclaringType)
        
    let fltDllImport (m:MemberFilterContext) =
        Option.fromBool DllImport (m.Member :? MethodDefinition && (m.Member :?> Mono.Cecil.MethodDefinition).HasPInvokeInfo)

    let fltIndexer (m:MemberFilterContext) =
        Option.fromBool Indexer (m.Member :? PropertyDefinition && (m.Member.Name = "Item" || m.Member.Name.EndsWith(".Item")))

    let fltOperator (m:MemberFilterContext) =
        Option.fromBool Operator (m.Member :? MethodDefinition && m.Member.IsSpecialName && m.Member.Name.StartsWith("op_"))

    let private isPublicOrFamilyMember (m:IMemberDefinition) =
        match m with
        | :? FieldDefinition as fd when fd.IsPublic || fd.IsFamily || fd.IsFamilyOrAssembly -> true
        | :? MethodDefinition as md when md.IsPublic || md.IsFamily || md.IsFamilyOrAssembly-> true
        | :? EventDefinition as ed when
            ed.AddMethod <> null && (ed.AddMethod.IsPublic || ed.AddMethod.IsFamily || ed.AddMethod.IsFamilyOrAssembly) 
            || ed.RemoveMethod <> null && (ed.RemoveMethod.IsPublic || ed.RemoveMethod.IsFamily || ed.RemoveMethod.IsFamilyOrAssembly)
            || ed.InvokeMethod <> null && (ed.InvokeMethod.IsPublic || ed.InvokeMethod.IsFamily || ed.InvokeMethod.IsFamilyOrAssembly) -> true
        | _ -> false
        
    let private isPublicMemberInVisibleType (m:MemberFilterContext) =
        isPublicOrFamilyMember m.Member
        && (TypeDefinition.isPublicVisible m.Member.DeclaringType)
        && m.AssemblyObfuscationLevel <> PrivateAndPublic

    let fltPublicMember (m:MemberFilterContext) =
        Option.fromBool PublicMember (isPublicMemberInVisibleType m)
        
    let fltRuntimeSpecialName (m:MemberFilterContext) =
        Option.fromBool RuntimeSpecialName m.Member.IsRuntimeSpecialName