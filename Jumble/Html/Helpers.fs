namespace Jumble.Html

module Helpers = 
    open FsHtml
    open Mono.Cecil
    
    open Jumble.Cecil

    type private TypeType = 
        | Builtin of string 
        | Other of string

    type TypeNameForm = 
    | Short
    | Long
    
    let private concatElements (e:Element) (xs:Element list) = 
        match xs with 
        | [] -> []
        | [_] -> xs
        | h::t -> [ 
            yield h 
            for item in t do 
                yield e
                yield item
            ]
    
    let private typeCls = ["class" %= "type"]
    let private builtinTypeCls = ["class" %= "type type-builtin"]

    let rec toString (elem:Element) =
        let attrToString = function 
            // f... injection
            | Attr (name, value) -> sprintf "%s=\"%s\"" name value
            
        match elem with 
        | Element (name, attrs, children) -> 
            match children with 
            | [] -> sprintf "<%s %s />" name (attrs |> List.map attrToString |> String.concat " ")
            | _ -> sprintf "<%s %s>%s</%s>" name (attrs |> List.map attrToString |> String.concat " ") (children |> List.map toString |> String.concat "") name
        | Text t -> t

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
            | Builtin tn -> span builtinTypeCls %tn
            | Other tn -> span typeCls %tn
    
        match t with 
        | :? ArrayType as t -> 
            span typeCls [
                reccall t.ElementType
                Text (sprintf "[%s]" (String.replicate (t.Rank-1) ","))              
            ]
        | :? ByReferenceType as t -> 
            span [] [ 
                span builtinTypeCls %"ref "
                reccall t.ElementType
            ]
    
        | :? GenericInstanceType as t -> 
            let genArgs = t.GenericArguments |> Seq.map reccall |> Seq.toList
            span typeCls [
                typeName2Html t.ElementType
                Text "<"
                span [] (concatElements (Text ",") genArgs)
                Text ">"
            ]
        | :? GenericParameter as t -> 
            span ["class" %= "genparam"] %t.Name

        | :? PointerType as t -> 
            span typeCls [
                reccall t.ElementType
                Text "*"
            ]
        | :? TypeDefinition as t -> 
            typeName2Html t
    
        | _ when t.GetType() = typeof<TypeReference> -> 
            typeName2Html t
    
        | _ -> span typeCls %t.FullName
    
    let private method2Html (m:MethodDefinition) = 
        span ["class" %= "method"] [
            type2Html Short m.ReturnType 
            Text " "
            span typeCls %m.DeclaringType.FullName
            Text (sprintf "::%s" m.Name)
            Text "("
            span [] (m.Parameters |> Seq.map (fun p -> type2Html Short p.ParameterType) |> Seq.toList |> concatElements (Text ", "))
            Text ")"
        ]
    
    let private property2Html (p:PropertyDefinition) = 
        span ["class" %= "property"] [
            type2Html Short p.PropertyType
            Text " "
            span typeCls %p.DeclaringType.FullName
            Text (sprintf "::%s" p.Name)
        ]
    
    let member2Html (m:IMemberDefinition) =
        match m with 
        | :? MethodDefinition as m -> method2Html m
        | :? PropertyDefinition as p -> property2Html p
        | _ -> failwith "not supported"