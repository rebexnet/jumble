namespace Jumble

type ArrayDimension = int

type SimpleParameterType = 
| SimpleType of string
| GenericType of string

/// Derived parameter of underlying type (e.g. array, pointer, byref)
type WrappedParameter = 
    | ArrayParameter of Parameter * ArrayDimension
    | PointerParameter of Parameter
    | ByRefParameter of Parameter
/// Parameter (Simple ('int' or 'T'), Generic ('IEnumerable<...>') or Wrapped ('int[]', 'ref int', ...)     
and Parameter = 
    | SimpleParameter of SimpleParameterType
    | GenericParameter of SimpleParameterType * Parameter list
    | WrappedParameter of WrappedParameter

module rec Parameter = 
    open Mono.Cecil

    let private typeRefToSimple (tr:TypeReference) = 
        match tr with 
        | :? GenericParameter as gp -> SimpleParameterType.GenericType gp.Name
        | _ -> SimpleType (TypeDefinitionName.nameFromTypeReference tr)

    let rec fromTypeReference (tr:TypeReference) = 
        match tr with 
        | :? GenericInstanceType as t -> Parameter.GenericParameter (typeRefToSimple t.ElementType, t.GenericArguments |> Seq.mapList fromTypeReference)
        | :? ArrayType as t -> toArray t.Rank (fromTypeReference t.ElementType)
        | :? ByReferenceType as t -> toRef (fromTypeReference t.ElementType)
        | :? PointerType as t -> toPointer (fromTypeReference t.ElementType)
        | _ -> SimpleParameter (SimpleType (TypeDefinitionName.nameFromTypeReference tr))

    let toArray dim pt = WrappedParameter <| ArrayParameter (pt, dim)
    let toRef pt = WrappedParameter <| ByRefParameter pt
    let toOut pt = WrappedParameter <| ByRefParameter pt
    let toPointer pt = WrappedParameter <| PointerParameter pt
    let toSimple name = name |> SimpleType |> SimpleParameter
    let toGeneric name pts = Parameter.GenericParameter (name |> SimpleType, pts)
    let toGenericType name = Parameter.SimpleParameter (GenericType name)

    let rec toString (p:Parameter) = 
        let simpleToString (sp:SimpleParameterType) = 
            match sp with 
            | SimpleType t -> t
            | GenericType t -> t

        match p with 
        | SimpleParameter sp -> simpleToString sp
        | GenericParameter (gp, args) -> 
            sprintf "%s<%s>" (simpleToString gp) (args |> List.map toString |> String.concat ", ")
        | WrappedParameter wp -> 
            match wp with 
            | ArrayParameter (par, dim) -> sprintf "%s[%s]" (toString par) (String.replicate (dim - 1) ",")
            | ByRefParameter par -> $"ref %s{toString par}"
            | PointerParameter par -> $"%s{toString par}*"
                
                    
    


type NamedParameter = {
    Name: string option
    Type: Parameter
}

module NamedParameter = 
    open Mono.Cecil

    let create name t = { Name = Some name; Type = t }
    let createUnnamed t = { Name = None; Type = t}
    let fromParameterDefinition (p:ParameterDefinition) : NamedParameter = 
        create p.Name (Parameter.fromTypeReference p.ParameterType)
    
    let toString np = 
        let pString = Parameter.toString np.Type
        match np.Name with 
        | None -> pString
        | Some n -> $"%s{pString} %s{n}"
    
    
//module Parameter = 
//    let private createSimple (typeResolver:TypeReference -> TypeTreeNode) (tr:TypeReference) : SimpleParameterType<TypeTreeNode> = 
//        match tr with 
//        | :? Mono.Cecil.GenericParameter as t -> SimpleParameterType.GenericType t.Name
//        | :? Mono.Cecil.TypeDefinition as t -> SimpleParameterType.SimpleType (typeResolver t)
//        | _ when tr.GetType() = typeof<TypeReference> -> SimpleParameterType.SimpleType (typeResolver tr)
//        | _ -> failwith "not supported"

//    let rec create (typeResolver:TypeReference -> TypeTreeNode) (tr:TypeReference) = 
//        let recursive = create typeResolver
//        match tr with 
//        | :? ArrayType as t -> 
//            let underlying = recursive t.ElementType
//            WrappedParameter (ArrayParameter (underlying, t.Rank))
//        | :? ByReferenceType as t -> 
//            let underlying = recursive t.ElementType
//            WrappedParameter (ByRefParameter underlying)
//        | :? GenericInstanceType as t -> 
//            let genericParams = t.GenericArguments |> Seq.map recursive |> Seq.toList
//            let elType = createSimple typeResolver t.ElementType
//            GenericParameter (elType, genericParams)
//        | :? GenericParameter as t -> GenericParameter ((createSimple typeResolver t), [])
//        | :? TypeDefinition | _ when tr.GetType() = typeof<TypeReference> -> SimpleParameter (SimpleType (typeResolver tr))
//        | _ -> failwith "not supported"
           
//    let private simpleToString = function SimpleType ttn -> TypeTreeNode.fullName ttn | GenericType tName -> tName
            
