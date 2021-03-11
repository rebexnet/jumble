namespace Jumble

open System.Collections.Generic
open Mono.Cecil

[<RequireQualifiedAccess>]
module TypeReference =
    let safeResolve (tr:TypeReference) =
        match tr.Resolve() with null -> failwithf "Unable to resolve type %s" tr.FullName | res -> res

    /// compares t1 and t2
    let rec areEqual (t1:TypeReference) (t2:TypeReference) : bool =
        if t1 = t2 then true else
        if t1 = null && t2 = null then true else
        if t1 = null || t2 = null then false else
        if (t1 :? TypeDefinition) || (t2 :? TypeDefinition) then t1.Resolve() = t2.Resolve() else
        if t1.GetType() <> t2.GetType() then false else
        match t1, t2 with
        | _ when t1.GetType() = typeof<TypeReference> -> t1.Resolve() = t2.Resolve()

        | (:? GenericInstanceType as git1), (:? GenericInstanceType as git2) ->
            areEqual git1.ElementType git2.ElementType
            && Seq.forall2 (areEqual) git1.GenericArguments git2.GenericArguments

        | (:? GenericParameter as gp1), (:? GenericParameter as gp2) ->
            gp1.Position = gp2.Position
            && gp1.Type = gp2.Type

        | (:? ArrayType as a1), (:? ArrayType as a2) ->
            a1.Rank = a2.Rank
            && areEqual a1.ElementType a2.ElementType

        | (:? ByReferenceType as brt1), (:? ByReferenceType as brt2) ->
            areEqual brt1.ElementType brt2.ElementType

        | (:? PointerType as pt1), (:? PointerType as pt2) ->
            areEqual pt1.ElementType pt2.ElementType

        | (:? RequiredModifierType as rmt1), (:? RequiredModifierType as rmt2) ->
            areEqual rmt1.ElementType rmt2.ElementType
            && areEqual rmt1.ModifierType rmt2.ModifierType

        | _ -> failwithf "Type %s is not supported (%s)" (t1.GetType().Name) t1.FullName

[<RequireQualifiedAccess>]
module MethodReference =
    let safeResolve (mr:MethodReference) =
        match mr.Resolve() with null -> failwithf "Unable to resolve method %s" mr.FullName | res -> res

    // compares two method signatures, disregarding the name
    let compareParameters (m1:MethodReference) (m2:MethodReference) : bool =
        m1.Parameters.Count = m2.Parameters.Count
        && m1.GenericParameters.Count = m2.GenericParameters.Count
        && TypeReference.areEqual m1.ReturnType m2.ReturnType
        && Seq.forall2 (fun (p1:ParameterDefinition) (p2:ParameterDefinition) -> TypeReference.areEqual p1.ParameterType p2.ParameterType) m1.Parameters m2.Parameters


[<RequireQualifiedAccess>]
module rec TypeDefinition =
    let rec isDelegate (t:TypeDefinition) : bool =
        if t.FullName = "System.Delegate" then true
        elif t.BaseType = null then false else
        let baseType = t.BaseType.Resolve()
        if baseType = null then false else
        isDelegate baseType

    /// is type public or nested public in public
    let rec isPublicVisible (t:TypeDefinition) =
        t.IsPublic || (t.IsNestedPublic && isPublicVisible t.DeclaringType)

    let baseTypeSafe (t:TypeDefinition) = Option.ofObj t.BaseType |> Option.map TypeReference.safeResolve
    let existsMethod (t:TypeDefinition) n = filterMethod t n |> Seq.isEmpty |> not
    let existsField (t:TypeDefinition) n = t.Fields |> Seq.exists (fun f -> f.Name = n)
    let findField (t:TypeDefinition) n = t.Fields |> Seq.find (fun f -> f.Name = n)
    let filterMethod (t:TypeDefinition) n = t.Methods |> Seq.filter (fun m -> m.Name = n) |> Seq.toList
    let findMethodSingle (t:TypeDefinition) n = filterMethod t n |> List.exactlyOne

    let members (t:TypeDefinition) : IMemberDefinition seq = seq {
        yield! (t.Methods |> Seq.cast<IMemberDefinition>)
        yield! (t.Properties |> Seq.cast<IMemberDefinition>)
        yield! (t.Events |> Seq.cast<IMemberDefinition>)
        yield! (t.Fields |> Seq.cast<IMemberDefinition>)
    }

    /// Gets all nested types of a specified type
    let rec nestedTypes (t:TypeDefinition) = seq {
        for nested in t.NestedTypes do
            yield nested
            yield! nestedTypes nested
    }

[<RequireQualifiedAccess>]
module ModuleDefinition =
    /// Gets all types in a module, including nested types
    let allTypes (m:ModuleDefinition) = seq {
        for t in m.Types do
            yield t
            yield! TypeDefinition.nestedTypes t
    }

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    /// Gets all types in an assembly, including nested types
    let allTypes (a:AssemblyDefinition) =
        a.Modules |> Seq.collect ModuleDefinition.allTypes

[<RequireQualifiedAccess>]
module MemberDefinition =
    // Name without any prefix (e.g. for property getter/setter)
    let canonicalName (m:IMemberDefinition) =
        match m with
        // todo: this is too fragile
        | :? MethodDefinition as m when m.IsGetter || m.IsSetter -> m.Name.Substring(4)
        | _ -> m.Name

    let parameters (m:IMemberDefinition) : ICollection<ParameterDefinition> =
        match m with
        | :? MethodDefinition as md -> upcast md.Parameters
        | :? PropertyDefinition as pd -> upcast pd.Parameters
        | _ -> upcast Array.empty<ParameterDefinition>

    let isPublic (m:IMemberDefinition) =
        match m with
        | :? EventDefinition as ed -> ed.AddMethod.IsPublic
        | :? FieldDefinition as fd -> fd.IsPublic
        | :? MethodDefinition as md -> md.IsPublic
        | :? PropertyDefinition as pd -> (pd.SetMethod <> null && pd.SetMethod.IsPublic) || (pd.GetMethod <> null && pd.GetMethod.IsPublic)
        | _ -> failwithf "%s is not supported" (m.GetType().FullName)