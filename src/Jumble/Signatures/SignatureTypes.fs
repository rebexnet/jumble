namespace Jumble

open Mono.Cecil

type ClassName = {
    ContainingClass: ClassName option
    Name: string
    GenericParameters: int
}
    
module ClassName = 

    let create name genpars = 
        { ClassName.ContainingClass = None; Name = name; GenericParameters = genpars }
    let rec fromTypeDefinition (td:TypeDefinition) = 
        { ClassName.ContainingClass = if td.DeclaringType = null then None else Some (fromTypeDefinition td.DeclaringType)
          Name = TypeDefinitionName.fullNameFromTypeReference td
          GenericParameters = td.GenericParameters.Count }
    let rec fromFullname (fullName:string) = 
        let toGenArgs (ga:string) = if ga = "" then 0 else int (ga.[1..])
        match fullName.IndexOf('/') with
        | -1 -> 
            match fullName with 
            | Regex @"^(.+)\.([^`.]+)(`[\d]+)?$" [namespc; clsname; genargs] -> 
                { ClassName.ContainingClass = None; Name = TypeDefinitionName.joinNamespaceS namespc clsname; GenericParameters = toGenArgs genargs }
            | Regex @"^([^`.]+)(`[\d]+)?$" [clsName; genargs] -> 
                { ClassName.ContainingClass = None; Name = clsName; GenericParameters = toGenArgs genargs }
            | _ -> failwithf $"Class name '%s{fullName}' is not supported"
        | n ->
            { fromFullname fullName.[n+1..] with ContainingClass = Some <| fromFullname (fullName.[..n-1]) }
              
    let rec toString n = 
        let genPars = if n.GenericParameters = 0 then "" else $"`%i{n.GenericParameters}"
        let cont = n.ContainingClass |> Option.map (fun c -> $"%s{toString c}/") |> Option.defaultValue ""
        
        $"%s{cont}%s{n.Name}%s{genPars}"

type MethodSignature = {
    ContainingClass: ClassName
    GenericParameters: string list
    Name: string
    Parameters: NamedParameter list
    ReturnType: Parameter option
}

module MethodSignature = 
    let create cls name genPars pars retPar = 
        { ContainingClass = cls; Name = name; GenericParameters = genPars; Parameters = pars; ReturnType = retPar }
    let toString (ms:MethodSignature) = 
        let genpars = match ms.GenericParameters with [] -> "" | pars -> sprintf "<%s>" (String.concat ", " pars)
        let parameters = ms.Parameters |> List.map NamedParameter.toString |> String.concat ", "
        $"%s{ClassName.toString ms.ContainingClass}.%s{ms.Name}%s{genpars}(%s{parameters})"

type PropertySignature = {
    ContainingClass: ClassName
    Name: string
    PropertyType: Parameter
}

type EventSignature = {
    ContainingClass: ClassName
    Name: string
    EventType: Parameter
}

type FieldSignature = {
    ContainingClass: ClassName
    Name: string
    FieldType: Parameter
}

type MemberSignature =
| MethodS of MethodSignature
| PropertyS of PropertySignature
| EventS of EventSignature
| FieldS of FieldSignature

// meh ugly
module MemberSignature = 
    let containingClass s = 
        match s with 
        | MethodS x -> x.ContainingClass
        | PropertyS x -> x.ContainingClass
        | EventS x -> x.ContainingClass
        | FieldS x -> x.ContainingClass

    let create (m:IMemberDefinition) : MemberSignature =
        let clsName = ClassName.fromTypeDefinition m.DeclaringType
        match m with
        | :? MethodDefinition as m ->
            let genPars = m.GenericParameters |> Seq.mapList (fun gp -> gp.Name)
            let pars = m.Parameters |> Seq.mapList NamedParameter.fromParameterDefinition
            let retPar = Parameter.fromTypeReference m.ReturnType
            MethodSignature.create clsName m.Name genPars pars (Some retPar) |> MethodS
        | :? PropertyDefinition as p ->
            let propType = Parameter.fromTypeReference p.PropertyType
            { PropertySignature.Name = p.Name; ContainingClass = clsName; PropertyType = propType } |> PropertyS
        | :? EventDefinition as e ->
            let evType = Parameter.fromTypeReference e.EventType
            { EventSignature.Name = e.Name; ContainingClass = clsName; EventType = evType } |> EventS
        | :? FieldDefinition as f ->
            let fType = Parameter.fromTypeReference f.FieldType
            { FieldSignature.Name = f.Name; ContainingClass = clsName; FieldType = fType } |> FieldS
        | _ -> failwithf $"%A{m} is not supported"