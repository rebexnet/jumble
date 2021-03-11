module Jumble.Rename.Exclusion.ScanEnumToString
open Mono.Cecil
open Mono.Cecil.Cil
open Jumble.Analysis
open Jumble.Rename

// todo: currently only works on literals
/// Checks whether ToString() is called on a literal value
let fltEnumToString (resolveRef: MemberDefResolver) (m:MemberFilterContext) : ExclusionScopeAndReason seq =
    match m.Member with
    | :? MethodDefinition as m when m.HasBody -> 
        m.Body.Instructions
        |> Seq.pairwise
        |> Seq.choose (fun (i1, i2) ->
            match i1.OpCode.Code, i2.OpCode.Code with
            | (Code.Box, Code.Callvirt) | (Code.Constrained, Code.Callvirt) ->
                let callMethodRef = i2.Operand :?> MethodReference  
                if callMethodRef.Name <> "ToString" || callMethodRef.DeclaringType.FullName <> "System.Object" then None else
                
                let objRef = i1.Operand :?> TypeReference |> resolveRef :?> TypeDefinition
                if objRef = null || objRef.IsEnum = false then None else
                    
                Some (ExclusionScopeAndReason.createType objRef AppliesToAllMembers StringConversion)
            | _ -> None
        )
    | _ -> Seq.empty