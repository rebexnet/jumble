namespace Jumble.Html

open System.Web

module Helpers =
    open Mono.Cecil
    
    type private TypeType =
        | Builtin of string 
        | Other of string

    type TypeNameForm = 
    | Short
    | Long

    let (~%) x = HttpUtility.HtmlEncode(x)

    let span content = $"<span>{content}</span>"
    let spanc cls content = $"<span class=\"{cls}\">{content}</span>"

    let private typeCls = "type"
    let private builtinTypeCls = "type type-builtin"

    let rec type2Html (form:TypeNameForm) (t:TypeReference)  = 
        let reccall = type2Html form
        let typeName2Html (t:TypeReference) = 
            let simplifiedName = 
                match t.FullName with 
                | "System.Boolean" -> Builtin "bool"
                | "System.Byte" -> Builtin "byte"
                | "System.Char" -> Builtin "char"
                | "System.Int32" -> Builtin "int"
                | "System.Object" -> Builtin "object"
                | "System.String" -> Builtin "string"
                | "System.Void" -> Builtin "void"
                | _ -> Other (match form with Short -> t.Name | Long -> t.FullName)

            match simplifiedName with
            | Builtin tn -> spanc builtinTypeCls %tn
            | Other tn -> spanc typeCls %tn
    
        match t with 
        | :? ArrayType as t ->
            let content = $"""{reccall t.ElementType}[{String.replicate (t.Rank-1) ","}]"""
            spanc typeCls content
        | :? ByReferenceType as t ->
            let content = $"""{spanc builtinTypeCls "ref "}{reccall t.ElementType}"""
            span content
    
        | :? GenericInstanceType as t -> 
            let genArgs = t.GenericArguments |> Seq.map reccall |> Seq.toList |> String.concat ", "
            let content = $"""{typeName2Html t.ElementType}&lt;{span genArgs}&gt;"""
            spanc typeCls content
        | :? GenericParameter as t -> 
            spanc "genparam" %t.Name

        | :? PointerType as t -> 
            spanc typeCls $"{reccall t.ElementType}*"
        | :? TypeDefinition as t ->
            typeName2Html t
    
        | _ when t.GetType() = typeof<TypeReference> -> 
            typeName2Html t
    
        | _ -> spanc typeCls %t.FullName
    
    let private method2Html (m:MethodDefinition) =
        let parameters = (m.Parameters |> Seq.map (fun p -> type2Html Short p.ParameterType) |> Seq.toList |> String.concat ", ")
        spanc "method" $"""{type2Html Short m.ReturnType} {spanc typeCls %m.DeclaringType.FullName}::{%m.Name}({span parameters})"""

    let private property2Html (p:PropertyDefinition) = 
        spanc "property" $"""{type2Html Short p.PropertyType} {spanc typeCls %p.DeclaringType.FullName}::{%p.Name}"""

    let member2Html (m:IMemberDefinition) =
        match m with 
        | :? MethodDefinition as m -> method2Html m
        | :? PropertyDefinition as p -> property2Html p
        | _ -> failwith "not supported"