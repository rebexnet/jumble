namespace Jumble.Analysis


[<RequireQualifiedAccess>]
module Deriver =
    open Jumble
    open Mono.Cecil
    open System.Diagnostics

    // gets corresponding type (generic or specific) in child type (going down the inheritance chain)
    let private deriveGenericParameter (gp:GenericParameter) (targetToSourceRef:GenericInstanceType) : TypeReference = 
        if gp.DeclaringType <> null then
            let declTypeDef = TypeReference.safeResolve gp.DeclaringType
            Debug.Assert (TypeReference.safeResolve targetToSourceRef = declTypeDef)
            let gpPos = gp.Position
            Debug.Assert (declTypeDef.GenericParameters[gpPos] = gp)
        
            targetToSourceRef.GenericArguments[gpPos]
        else if gp.DeclaringMethod <> null then 
            upcast gp
        else failwith "not supported"
    
    // gets corresponding type in child type (going down the inheritance chain)
    let rec deriveType (targetToOriginRef:TypeReference) (parameterType:TypeReference) : TypeReference =
        assert(parameterType <> null)
        
        let deriveByRef = deriveType targetToOriginRef

        match parameterType with 
        | :? ArrayType as atp -> 
            match deriveByRef atp.ElementType with 
            | et when et = atp.ElementType -> upcast atp
            | et -> upcast ArrayType(et, atp.Rank)

        | :? ByReferenceType as t -> 
            match deriveByRef t.ElementType with 
            | et when et = t.ElementType -> upcast t
            | et -> upcast ByReferenceType(et)

        | :? GenericInstanceType as git ->
            let derivedArgs = git.GenericArguments |> Seq.map deriveByRef |> Seq.toList
            let resolvedParameter = TypeReference.safeResolve parameterType
            let newGit = GenericInstanceType(resolvedParameter)
            derivedArgs |> List.iter newGit.GenericArguments.Add
            upcast newGit        
        
        | :? GenericParameter as gp -> 
            match gp.Type with 
            | GenericParameterType.Type -> deriveGenericParameter gp (targetToOriginRef :?> GenericInstanceType)
            | GenericParameterType.Method -> upcast gp
            | _ -> failwith "not supported"

        | :? PointerType as pt -> 
            match deriveByRef pt.ElementType with 
            | et when et = pt.ElementType -> upcast pt
            | et -> upcast PointerType(et)
        
        | :? RequiredModifierType as t ->
            match (deriveByRef t.ElementType, deriveByRef t.ModifierType) with
            | et, mt when et = t.ElementType && mt = t.ModifierType -> upcast t
            | et, mt -> upcast RequiredModifierType(mt, et)
            
        | :? TypeDefinition -> parameterType

        // typically unsafe code from C++/CLI, modopt(modifier_type)
        // these types should not have derivatives, but to be on the safe side, we derive them
        | :? OptionalModifierType as t ->
            match (deriveByRef t.ElementType, deriveByRef t.ModifierType) with
            | et, mt when et = t.ElementType && mt = t.ModifierType -> upcast t
            | et, mt -> upcast OptionalModifierType(mt, et)

        | _ when parameterType.GetType() = typeof<TypeReference> -> parameterType

        | _ -> failwithf
                   $"Type %s{parameterType.GetType().FullName} is not supported (%s{parameterType.FullName}) (type reference %s{targetToOriginRef.FullName})"

    let deriveMethod (target:TypeDefinition) (parentRef:TypeReference) (m:MethodReference) : MethodReference = 
        let mapParam = deriveType parentRef
        let returnType = mapParam m.ReturnType
           
        let result = if target = null then 
                        MethodReference(m.Name, returnType) 
                     else 
                        MethodReference(m.Name, returnType, target)

        m.Parameters 
        |> Seq.map (fun p -> mapParam p.ParameterType) 
        |> Seq.map (fun p -> ParameterDefinition(p))
        |> Seq.iter result.Parameters.Add
            
        // keeping original object causes really odd behavior where written assembly does not contain generic parameter name but !!0 instead
        // this causes problems even for ILVerify
        m.GenericParameters 
        |> Seq.map (fun gp -> GenericParameter(gp.Name, gp.Owner))
        |> Seq.iter result.GenericParameters.Add

        result