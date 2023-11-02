module Jumble.Analysis.InterfaceMethodImplSearch

open Mono.Cecil
open Jumble

// Example:
//
// interface I<T> { void M(T t); }
// class C : I<int>, I<string> {
//   void M(int t) { }
//   void M(string t) { }
// }
//
// Method definition is the I<T> interface's M(T) method.
// Interface reference is one of the I<int> or I<string>.
// The method signature is one of the M(int) or M(string).
type private InterfaceMethodDetails = {
    // Definition of the interface method
    methodDef: MethodDefinition

    // Signature of the method on the original implementor
    methodSig: MethodReference

    // Reference to the interface from the original implementor
    interfaceRef: TypeReference
}
with
    static member create(interfaceRef: TypeReference, methodDef: MethodDefinition) =
        {
            methodDef = methodDef
            interfaceRef = interfaceRef
            methodSig = DeriveDown.deriveMethod interfaceRef methodDef
        }

    /// Gets a value indicating whether the given method is explicit implementation of the interface method.
    member this.matchesExplicit (deriver: TypeReference -> TypeReference) (m:MethodDefinition) =
        if m.IsStatic then false else
        let ovr = m.Overrides
                  |> Seq.filter (fun ovr -> MethodReference.safeResolve ovr = this.methodDef)
                  |> Seq.trySingle

        if ovr.IsNone then false else
        let ovr = ovr.Value

        let derivedDeclType = deriver ovr.DeclaringType
        if not (TypeReference.areEqual derivedDeclType this.interfaceRef) then false else

        let derivedMethod = DeriveDown.deriveMethodWithDeriver deriver m
        MethodReference.compareParameters derivedMethod this.methodSig

    /// Gets a value indicating whether the given method is implicit implementation of the interface method.
    member this.matchesImplicit (deriver: TypeReference -> TypeReference) (m:MethodDefinition) =
        if m.IsStatic then false else
        if m.Name <> this.methodDef.Name then false else
        if m.IsPublic = false then false else
        let derivedMethod = DeriveDown.deriveMethodWithDeriver deriver m
        MethodReference.compareParameters derivedMethod this.methodSig

// Deriver derives type from the context of the given type to the context of the original implementor
// (whose interface and method signatures are in InterfaceMethodDetails).
let rec private tryFind (d: InterfaceMethodDetails) (deriver: TypeReference -> TypeReference) (t:TypeDefinition) : MethodDefinition option =
    let findExplicit() = t.Methods |> Seq.filter (d.matchesExplicit deriver) |> Seq.trySingle
    let findImplicit() = t.Methods |> Seq.filter (d.matchesImplicit deriver) |> Seq.trySingle
    let findInBaseType() =
        match t.BaseType with
        | null -> None
        | baseType ->
            let baseD = DeriveDown.deriveType baseType >> deriver
            tryFind d baseD (TypeReference.safeResolve baseType)

    findExplicit()
    |> Option.orElseWith findImplicit
    |> Option.orElseWith findInBaseType

/// Finds interface method implementations on the given type.
/// Multiple results are returned if the type implements the same interface multiple times using different generic parameters.
/// !!! Since this function is only used for grouping, when type type is a class, it will NOT search for default implementation on an interface.
let findInterfaceMethodImplementations (interfaceMethodDefinition: MethodDefinition) (t:TypeDefinition) =
    let interfaceRefs = t.Interfaces
                        |> Seq.map(fun i -> i.InterfaceType)
                        |> Seq.filter(fun t -> TypeReference.safeResolve t = interfaceMethodDefinition.DeclaringType)
                        |> Seq.toList

    interfaceRefs
    |> List.map (fun t -> InterfaceMethodDetails.create(t, interfaceMethodDefinition))
    |> List.choose (fun d -> tryFind d id t)