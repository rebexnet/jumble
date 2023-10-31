namespace Jumble

open System.Collections.Generic
open Mono.Cecil

[<RequireQualifiedAccess>]
module TypeReference =
    let safeResolve (tr:TypeReference) =
        match tr.Resolve() with
        | null ->
            let assemblyName =
                match tr.Scope with
                | :? AssemblyNameReference as anr -> Some anr.FullName
                | _ -> None
                |> Option.map (fun n -> $" ({n})")
                |> Option.defaultValue ""

            match tr.DeclaringType with
            | null -> failwith $"Unable to resolve type %s{tr.FullName} ({assemblyName}) ({tr.GetType().FullName})"
            | dt -> failwithf $"Unable to resolve type %s{tr.FullName} ({assemblyName}) declared in %s{dt.FullName}"
        | res -> res

    /// compares t1 and t2
    let rec areEqual (t1:TypeReference) (t2:TypeReference) : bool =
        if t1 = t2 then true else
        if t1 = null && t2 = null then true else
        if t1 = null || t2 = null then false else
        if (t1 :? TypeDefinition) || (t2 :? TypeDefinition) then t1.Resolve() = t2.Resolve() else
        if t1.GetType() <> t2.GetType() then false else
        match t1, t2 with
        | _ when t1.GetType() = typeof<TypeReference> -> t1.Resolve() = t2.Resolve()

        | :? GenericInstanceType as git1, (:? GenericInstanceType as git2) ->
            areEqual git1.ElementType git2.ElementType
            && Seq.forall2 areEqual git1.GenericArguments git2.GenericArguments

        | :? GenericParameter as gp1, (:? GenericParameter as gp2) ->
            gp1.Position = gp2.Position
            && gp1.Type = gp2.Type

        | :? ArrayType as a1, (:? ArrayType as a2) ->
            a1.Rank = a2.Rank
            && areEqual a1.ElementType a2.ElementType

        | :? ByReferenceType as brt1, (:? ByReferenceType as brt2) ->
            areEqual brt1.ElementType brt2.ElementType

        | :? PointerType as pt1, (:? PointerType as pt2) ->
            areEqual pt1.ElementType pt2.ElementType

        | :? RequiredModifierType as rmt1, (:? RequiredModifierType as rmt2) ->
            areEqual rmt1.ElementType rmt2.ElementType
            && areEqual rmt1.ModifierType rmt2.ModifierType

        | :? OptionalModifierType as opt1, (:? OptionalModifierType as opt2) ->
            areEqual opt1.ElementType opt2.ElementType
            && areEqual opt1.ModifierType opt2.ModifierType

        | _ -> failwithf $"Type %s{t1.GetType().Name} is not supported (%s{t1.FullName})"

[<RequireQualifiedAccess>]
module FieldReference =
    let safeResolve (fr:FieldReference) =
        match fr.Resolve() with null -> failwithf $"Unable to resolve field %s{fr.FullName}" | res -> res


[<RequireQualifiedAccess>]
module MethodReference =
    let safeResolve (mr:MethodReference) =
        match mr.Resolve() with null -> failwithf $"Unable to resolve method %s{mr.FullName} declared in %s{mr.DeclaringType.FullName} (%s{mr.DeclaringType.Module.Assembly.Name.Name})" | res -> res

    // compares two method signatures, disregarding the name
    let compareParameters (m1:MethodReference) (m2:MethodReference) : bool =
        m1.Parameters.Count = m2.Parameters.Count
        && m1.GenericParameters.Count = m2.GenericParameters.Count
        && TypeReference.areEqual m1.ReturnType m2.ReturnType
        && Seq.forall2 (fun (p1:ParameterDefinition) (p2:ParameterDefinition) -> TypeReference.areEqual p1.ParameterType p2.ParameterType) m1.Parameters m2.Parameters


[<RequireQualifiedAccess>]
module rec TypeDefinition =
    // todo: why do we filter out delegates?
    let rec isDelegate (t:TypeDefinition) : bool =
        let isDel (t:TypeReference) = t.Namespace = "System" && (t.Name = "Delegate" || t.Name = "MulticastDelegate")
        if isDel t then true else if t.BaseType = null then false else isDel t.BaseType

    /// is type public or nested public in public
    let rec isPublicVisible (t:TypeDefinition) =
        t.IsPublic || (t.IsNestedPublic && isPublicVisible t.DeclaringType)

    let baseTypeSafe (t:TypeDefinition) = Option.ofObj t.BaseType |> Option.map TypeReference.safeResolve
    let existsMethod (t:TypeDefinition) n = filterMethod t n |> Seq.isEmpty |> not
    let existsField (t:TypeDefinition) n = t.Fields |> Seq.exists (fun f -> f.Name = n)
    let findField (t:TypeDefinition) n = t.Fields |> Seq.find (fun f -> f.Name = n)
    let filterMethod (t:TypeDefinition) n = t.Methods |> Seq.filter (fun m -> m.Name = n) |> Seq.toList
    let findMethodSingle (t:TypeDefinition) n = filterMethod t n |> List.exactlyOne

    let inline private memberTyped<'T> (t:TypeDefinition) : 'T seq = seq {
        yield! (t.Methods |> Seq.cast<'T>)
        yield! (t.Properties |> Seq.cast<'T>)
        yield! (t.Events |> Seq.cast<'T>)
        yield! (t.Fields |> Seq.cast<'T>)
    }

    let memberReferences = memberTyped<MemberReference>

    let memberDefinitions = memberTyped<IMemberDefinition>

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

    let inline parametersTyped<'T> (m:'T) : ICollection<ParameterDefinition> =
        match m :> obj with
        | :? MethodDefinition as md -> upcast md.Parameters
        | :? PropertyDefinition as pd -> upcast pd.Parameters
        | _ -> upcast Array.empty<ParameterDefinition>

    let parameters = parametersTyped<IMemberDefinition>

    let parameterNames (m:IMemberDefinition) =
        parameters m |> Seq.map (fun p -> p.Name) |> Seq.toList

    let isPublic (m:IMemberDefinition) =
        match m with
        | :? EventDefinition as ed -> ed.AddMethod.IsPublic
        | :? FieldDefinition as fd -> fd.IsPublic
        | :? MethodDefinition as md -> md.IsPublic
        | :? PropertyDefinition as pd -> (pd.SetMethod <> null && pd.SetMethod.IsPublic) || (pd.GetMethod <> null && pd.GetMethod.IsPublic)
        | _ -> failwithf $"%s{m.GetType().FullName} is not supported"

[<RequireQualifiedAccess>]
module MemberReference =
    let safeResolve (mr:MemberReference) =
        match mr.Resolve() with
        | null -> failwithf $"Unable to resolve %s{mr.GetType().Name} %s{mr.FullName}"
        | resolved -> resolved

    let isPublic (mr:MemberReference) = MemberDefinition.isPublic (mr :> obj :?> IMemberDefinition)
    let parameters = MemberDefinition.parametersTyped<MemberReference>