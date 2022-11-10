module Jumble.Export.Mapfile

open System.IO
open Jumble
open Jumble.Rename
open Mono.Cecil

type Format = CSharp

type TypeNameLookup = TypeReference -> TypeDefinitionName

module CSharpExport =
    let private typeShortName (t:TypeDefinitionName) =
        // todo: check for assembly to avoid possible shims
        match t.Namespace with
        | None -> t.FullName
        | Some ns ->
            match ns with
            | "System" ->
                match t.Name with
                | "Boolean" -> "bool"
                | "Char" -> "char"
                | "Double" -> "double"
                | "Int32" -> "int"
                | "Int64" -> "long"
                | "Object" -> "object"
                | "UInt32" -> "uint"
                | "UInt64" -> "ulong"
                | "Single" -> "float"
                | "String" -> "string"
                | "Void" -> "void"
                | _ -> t.Name
            | "System.Collections" | "System.Collections.Generic" -> t.Name
            | _ -> t.FullName

    // todo: tweak visibility keywords
    let typeSig (t:TypeDefinition) (name:TypeDefinitionName) =
        if t.IsClass then
            stringBuffer {
                if t.IsPublic then "public" else "private"
                if t.IsEnum then " enum " else " class "
                name.FullName
            }
        else if t.IsInterface then
            stringBuffer {
                if t.IsPublic then "public" else "private"
                " interface "
                name.FullName
            }
        else failwith "not supported"

    let memberSig (tl:TypeNameLookup) (t:IMemberDefinition) (name:string) (parameters:string list) =
        let visibility = if MemberDefinition.isPublic t then "public" else "private"

        // todo: add proper support for generic parameters
        // todo: proper resolution of generic return types, such as IEnumerable<string, T>
        let rec typeName (tr:TypeReference) =
            match tr with
            | :? GenericParameter as gp -> gp.Name
            | :? ByReferenceType as brt -> "&" + typeName brt.ElementType
            | :? ArrayType as at -> (typeName at.ElementType) + "[]"  // todo: multi-dimensional array
            | tr -> tl tr |> typeShortName

        match t with
        | :? MethodDefinition as md ->
            stringBuffer {
                $"{visibility} "
                if md.IsConstructor then
                    md.DeclaringType.Name
                else
                    $"{typeName md.ReturnType} {name}"
                if md.GenericParameters.Count > 0 then
                    let genpars = String.concat ", " (md.GenericParameters |> Seq.map typeName)
                    $"<{genpars}>"

                let pars = (md.Parameters, parameters) ||> Seq.map2 (fun p pName -> $"{typeName p.ParameterType} {pName}") |> String.concat ", "
                $"({pars})"
            }
        | :? FieldDefinition as fd ->
            $"{visibility} {typeName fd.FieldType} {name}"
        | :? PropertyDefinition as pd ->
            let getter = if pd.GetMethod = null then "" else "get; "
            let setter = if pd.SetMethod = null then "" else "set; "
            $"{visibility} {typeName pd.PropertyType} {name} {{ {getter}{setter}}}"
        | :? EventDefinition as ed ->
            $"{visibility} event {typeName ed.EventType} {name}"
        | x ->
            failwith $"type {x.GetType().FullName} is not supported"

let exportCSharp (output:TextWriter) (renameLookup:TypeDefinition -> TypeRenamePlan option) (plan:ModuleRenamePlan) (m:ModuleDefinition) =
    let writeln (s:string) = output.WriteLine(s)
    let write (s:string) = output.Write(s)

    let exportType (t:TypeDefinition) =
        let typeID = t.MetadataToken
        let renamePlan = plan.TypeRenamePlans |> Array.tryFind (fun p -> p.TypeID.MemberToken = typeID)
        let currentName = TypeDefinitionName.fromTypeDefinition t

        let originalName = renamePlan |> Option.map (fun p -> p.OriginalName) |> Option.defaultValue currentName
        let newName = renamePlan |> Option.map (fun p -> p.NewName) |> Option.defaultValue currentName

        writeln ""
        if originalName <> newName then
            write "// "
            writeln <| CSharpExport.typeSig t originalName

        write <| CSharpExport.typeSig t newName
        writeln " {"

    let renamedTypeNameLookup (tr:TypeReference) =
        let td = TypeReference.safeResolve tr
        renameLookup td
        |> Option.map (fun p -> p.NewName)
        |> Option.defaultValue (TypeDefinitionName.fromTypeDefinition td)

    let originalTypeNameLookup (tr:TypeReference) =
        TypeReference.safeResolve tr |> TypeDefinitionName.fromTypeDefinition

    writeln $"// {m.Assembly.FullName}"
    m.Types
    |> Seq.sortBy (fun t -> t.FullName)
    |> Seq.iter (fun t ->
        exportType t
        TypeDefinition.memberDefinitions t
        |> Seq.iter (fun m ->
            let memberRenamePlan = plan.MemberRenamePlans |> Array.tryFind (fun p -> p.MemberID.MemberToken = m.MetadataToken)
            let originalParameters = MemberDefinition.parameterNames m
            let originalMemberSig = CSharpExport.memberSig originalTypeNameLookup m m.Name originalParameters
            let renamedName = memberRenamePlan |> Option.map (fun p -> p.NewName) |> Option.defaultValue m.Name
            let renamedParameters = memberRenamePlan |> Option.map (fun p -> p.NewParameters) |> Option.defaultValue originalParameters

            let renamedMemberSig = CSharpExport.memberSig renamedTypeNameLookup m renamedName renamedParameters

            if originalMemberSig <> renamedMemberSig then
                writeln $"  // {originalMemberSig};"
            writeln $"  {renamedMemberSig};"
        )
        writeln "}"
    )

    writeln ""