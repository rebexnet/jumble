namespace Jumble.Analysis

open Jumble
open Mono.Cecil

// note: calling .Resolve() is SLOW therefore we should NOT be using unless cached
type FieldResolver = FieldReference -> FieldDefinition
type MemberResolver = MemberReference -> IMemberDefinition
type MethodResolver = MethodReference -> MethodDefinition
type TypeResolver = TypeReference -> TypeDefinition

type Resolvers =
    {
        TypeResolver: TypeResolver
        MethodResolver: MethodResolver
        FieldResolver: FieldResolver
    }
    with
        member this.MemberResolver : MemberResolver =
            let f (m:MemberReference) : IMemberDefinition =
                match m with
                | :? TypeReference as tr -> upcast this.TypeResolver tr
                | :? FieldReference as fr -> upcast this.FieldResolver fr
                | :? MethodReference as mr -> upcast this.MethodResolver mr
                | _ -> failwithf $"Member reference type %s{m.GetType().Name} is not supported"
            f
        static member createSafeMemoized() =
            {
                TypeResolver = memoize TypeReference.safeResolve
                MethodResolver = memoize MethodReference.safeResolve
                FieldResolver = memoize FieldReference.safeResolve
            }

type MemberLookup = IMemberDefinition -> MemberReference seq
type TypeLookup = TypeDefinition -> TypeReference array
type MethodLookup = MethodDefinition -> MethodReference array
type FieldLookup = FieldDefinition -> FieldReference array

type Lookups = {
     MemberLookup: MemberLookup
     TypeLookup: TypeLookup
     MethodLookup: MethodLookup
     FieldLookup: FieldLookup
}
