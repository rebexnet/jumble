namespace Jumble.Analysis

open System
open System.Collections.Generic
open FSharpPlus
open Jumble
open Jumble.Utils
open Mono.Cecil

type AssemblyTreeNode (ad:AssemblyDefinition, freferences:unit -> AssemblyTreeNode list) as this =
    let mutable refs : AssemblyTreeNode list = List.empty<AssemblyTreeNode>
    let referencedBy = HashSet<AssemblyTreeNode>()
    let referencedByRec = HashSet<AssemblyTreeNode>()
    let referencesRec = HashSet<AssemblyTreeNode>()
    
    let updateReferenced() = 
        let rec addRefRec (node:AssemblyTreeNode) = 
            if node.ReferencedByRec.Add(this) then
                node.References |> Seq.iter addRefRec    
        
        refs |> Seq.iter (fun r -> 
            r.ReferencedBy.Add(this) |> ignore
            addRefRec r 
        )
        
    let updateReferencesRec() =
        let rec addRefRec (node:AssemblyTreeNode) =
            if referencesRec.Add(node) then node.References |> Seq.iter addRefRec
        
        addRefRec this
    
    member _.Assembly = ad
    member _.FinalizeRefs() =
        refs <- freferences()
        updateReferenced()
        updateReferencesRec()

    member _.ReferencedBy : HashSet<AssemblyTreeNode> = referencedBy
    member _.ReferencedByRec : HashSet<AssemblyTreeNode> = referencedByRec
    member _.References = refs
    member _.ReferencesRec = referencesRec
 
type TypeTreeNode (asmNode:AssemblyTreeNode, t:TypeDefinition, b: TypeTreeNode option, ifaces: TypeTreeNode[]) =
    let ancestors = match b with None -> [] | Some bt -> bt :: bt.Ancestors
    let children = ResizeArray<TypeTreeNode>()
    let descendants = lazy(Seq.append children (children |> Seq.collect (fun t -> t.Descendants)) |> Seq.distinct |> Seq.toList)
    let members = TypeDefinition.members t |> Seq.toList
    let inheritedInterfaces = 
        seq {
            yield! ifaces
            yield! ifaces |> Seq.collect (fun i -> i.Interfaces)
            yield! b |> Option.collect (fun i -> i.Interfaces |> Array.toSeq)
        } |> Seq.distinct |> Seq.toArray

    member _.Ancestors = ancestors
    member _.AssemblyTreeNode = asmNode
    member _.TypeDefinition = t
    member _.Base = b
    member _.Interfaces = ifaces
    member _.Children = children
    member _.Members = members
    member _.InheritedInterfaces = inheritedInterfaces
    
    override x.Equals obj = Object.ReferenceEquals(x, obj)
    override x.GetHashCode () = x.TypeDefinition.GetHashCode()
        
    member this.AncestorsAndSelf = this::this.Ancestors
        
    member _.Descendants with get() = descendants.Value
    member this.DescendantsAndSelf with get() = this::this.Descendants
        
    member _.FindMember name = members |> Seq.filter (fun m -> m.Name = name) |> Seq.exactlyOne

    override this.ToString() = this.TypeDefinition.FullName
    
    interface IComparable with 
        member this.CompareTo obj = 
            match obj with 
            | :? TypeTreeNode as ttn -> 
                if this = ttn then 0 else compare this.TypeDefinition.FullName ttn.TypeDefinition.FullName
            | _ -> -1

    static member fullName (ttn:TypeTreeNode) = ttn.TypeDefinition.FullName

    static member isAttributeType (t:TypeTreeNode) =
        t.AncestorsAndSelf |> List.exists (fun t -> t.TypeDefinition.FullName = "System.Attribute")

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