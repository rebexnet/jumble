namespace Jumble.Cecil

open System.Text
open Mono.Cecil
open Printf

module CSharpExport =
    module private PropertyDefinition =
        let isPublic (p:PropertyDefinition) =
            (p.SetMethod <> null && p.SetMethod.IsPublic) || (p.GetMethod <> null && p.GetMethod.IsPublic)

        let toCSharpSignature (p:PropertyDefinition) =
            let isPropPublic = isPublic p 
            
            let sb = StringBuilder()
            bprintf sb (if isPropPublic then "public %s { " else "private %s { ") p.Name
            if p.GetMethod <> null then
                bprintf sb (if isPropPublic && p.GetMethod.IsPublic = false then "private get; " else "get; ")
            
            if p.SetMethod <> null then
                bprintf sb (if isPropPublic && p.SetMethod.IsPublic = false then "private set; " else "set; ")

            bprintf sb "}"
            
            sb.ToString()
            
    let private typeVisibility (t:IMemberDefinition) =
        match t with
        | :? TypeDefinition as t -> 
            if t.IsPublic || t.IsNestedPublic then "public" else
            if t.IsNested then "private" else "internal"
        | :? MethodDefinition as m ->
            if m.IsPublic then "public" else if m.IsPrivate then "private" else "protected"
        | :? EventDefinition as e ->
            if (e.AddMethod <> null && e.AddMethod.IsPublic) || (e.RemoveMethod <> null && e.RemoveMethod.IsPublic) then "public" else "private"
        | :? FieldDefinition as fd ->
            if fd.IsPublic then "public" else if fd.IsPrivate then "private" else "protected"
        | _ -> "other"

    /// Still very WIP    
    let toCSharpSignature (m:IMemberDefinition) : string =
        let indent = "    "
        match m with
        | :? TypeDefinition as td when td.IsClass -> sprintf "%s class %s" (typeVisibility td) td.FullName
        | :? TypeDefinition as td when td.IsEnum -> sprintf "%s enum %s" (typeVisibility td) td.FullName
        | :? TypeDefinition as td when td.IsValueType -> sprintf "%s struct %s" (typeVisibility td) td.FullName
        | :? EventDefinition as ed -> indent + (sprintf "%s event %s;" (typeVisibility ed) ed.Name) 
        | :? FieldDefinition as fd -> indent + (sprintf "%s %s;" (typeVisibility fd) fd.Name)
        | :? MethodDefinition as md -> indent + (sprintf "%s %s(%i)" (typeVisibility md) md.Name md.Parameters.Count)
        | :? PropertyDefinition as pd -> indent + PropertyDefinition.toCSharpSignature pd
        | m -> indent + (sprintf "??? %s %s" (typeVisibility m) m.Name) 
