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

type MemberReferenceLookup = IMemberDefinition -> MemberReference seq
type TypeReferenceLookup = TypeDefinition -> TypeReference array
type MethodReferenceLookup = MethodDefinition -> MethodReference array
type FieldReferenceLookup = FieldDefinition -> FieldReference array
type MVID = System.Guid
type MemberID = { MemberToken: MetadataToken; MVID: MVID }
with
    static member fromDefinition (def:IMemberDefinition) =
        {
            MemberToken = def.MetadataToken
            MVID = (def :?> MemberReference).Module.Mvid
        }

type MemberIDLookup = MemberID -> IMemberDefinition
type TypeIDLookup = MemberID -> TypeDefinition

type Lookups = {
     MemberLookup: MemberReferenceLookup
     TypeLookup: TypeReferenceLookup
     MethodLookup: MethodReferenceLookup
     FieldLookup: FieldReferenceLookup
     MemberIDLookup: MemberIDLookup
}
with member this.TypeIDLookup (id:MemberID) = this.MemberIDLookup id :?> TypeDefinition