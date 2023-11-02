namespace Jumble.Analysis

open Jumble
open Mono.Cecil
open System.Diagnostics

/// Derives types and members - update their reference signature, changing types (parameters, return, generic) so they match the child type

// !! Deriving up would result in combinatory explosion, so we avoid it
// For example, reference IFoo<int> to interface IFoo<T> and method `int Foo(int value)` could be derived to:
// - T Foo(T)
// - int Foo(T)
// - T Foo(int)
// - int Foo(int)
// With more generic arguments and/or method parameters the number of combinations would grow exponentially

module DeriveDown =
    let private deriveGeneric (parentRef:GenericInstanceType) (gp:GenericParameter) : TypeReference =
        if gp.DeclaringType <> null then
            let declTypeDef = TypeReference.safeResolve gp.DeclaringType
            Debug.Assert (TypeReference.safeResolve parentRef = declTypeDef)
            let gpPos = gp.Position
            Debug.Assert (declTypeDef.GenericParameters[gpPos] = gp)

            parentRef.GenericArguments[gpPos]
        else if gp.DeclaringMethod <> null then
            upcast gp
        else failwith "not supported"

    /// Gets type corresponding to 't', derived by going down the inheritance chain 'parentRef' (parentRef points from child to parent)
    let rec deriveType (parentRef: TypeReference) (t: TypeReference) : TypeReference =
        assert(t <> null)
        assert(parentRef <> null)

        let deriveByRef = deriveType parentRef

        match t with
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
            let resolvedParameter = TypeReference.safeResolve t
            let newGit = GenericInstanceType(resolvedParameter)
            derivedArgs |> List.iter newGit.GenericArguments.Add
            upcast newGit

        | :? GenericParameter as gp ->
            match gp.Type with
            | GenericParameterType.Type -> deriveGeneric (parentRef :?> GenericInstanceType) gp
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

        | :? TypeDefinition -> t

        // typically unsafe code from C++/CLI, modopt(modifier_type)
        // these types should not have derivatives, but to be on the safe side, we derive them
        | :? OptionalModifierType as t ->
            match (deriveByRef t.ElementType, deriveByRef t.ModifierType) with
            | et, mt when et = t.ElementType && mt = t.ModifierType -> upcast t
            | et, mt -> upcast OptionalModifierType(mt, et)

        | _ when t.GetType() = typeof<TypeReference> -> t

        | _ -> failwithf
                   $"Type %s{t.GetType().FullName} is not supported (%s{t.FullName}) (type reference %s{parentRef.FullName})"

    let deriveMethodWithDeriver (deriveType: TypeReference -> TypeReference) (m:MethodReference) =
        let returnType = deriveType m.ReturnType

        let result = MethodReference(m.Name, returnType)

        m.Parameters
        |> Seq.map (fun p -> deriveType p.ParameterType)
        |> Seq.map (fun p -> ParameterDefinition(p))
        |> Seq.iter result.Parameters.Add

        // keeping original object causes really odd behavior where written assembly does not contain generic parameter name but !!0 instead
        // this causes problems even for ILVerify
        m.GenericParameters
        |> Seq.map (fun gp -> GenericParameter(gp.Name, gp.Owner))
        |> Seq.iter result.GenericParameters.Add

        result

    let deriveMethod (parentRef:TypeReference) (m:MethodReference) : MethodReference =
        let deriveType = deriveType parentRef
        deriveMethodWithDeriver deriveType m

    let deriveMethodWithTarget (parentRef:TypeReference) (m:MethodReference) (target: TypeDefinition) : MethodReference =
        assert (target <> null)

        let result = deriveMethod parentRef m
        result.DeclaringType <- target
        result