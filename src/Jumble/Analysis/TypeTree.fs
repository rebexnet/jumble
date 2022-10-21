namespace Jumble

open FSharp.Core.Fluent
open System.Collections.Generic

open FSharpPlus
open Jumble
open Mono.Cecil
open Serilog
open System

type TypeTreeNode (asm:AssemblyTreeNode, t:TypeDefinition, b: TypeTreeNode option, ifaces: TypeTreeNode[]) =
    let ancestors = match b with None -> [] | Some bt -> bt :: bt.Ancestors
    let children = ResizeArray<TypeTreeNode>()
    let descendants = lazy(Seq.append children (children |> Seq.collect (fun t -> t.Descendants)) |> Seq.distinct |> Seq.toList)
    let members = TypeDefinition.members t |> Seq.toList
    member _.Ancestors = ancestors
    member _.Assembly = asm
    member _.TypeDefinition = t
    member _.Name = TypeDefinitionName.fromTypeDefinition t
    member _.Base = b
    member _.Interfaces = ifaces
    member _.IsClass with get() = t.IsClass
    member _.IsInterface with get() = t.IsInterface
    member _.Children = children
    member _.Members = members
    override x.Equals obj = Object.ReferenceEquals(x, obj)
    override _.GetHashCode () = t.GetHashCode()

    member this.AncestorsAndSelf = this::this.Ancestors

    member _.Descendants with get() = descendants.Value

    member _.FindMember name = members |> Seq.filter (fun m -> m.Name = name) |> Seq.exactlyOne

    override _.ToString() = t.FullName

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? TypeTreeNode as ttn ->
                if this = ttn then 0 else
                let asmCompare = compare this.Assembly ttn.Assembly
                if asmCompare <> 0 then asmCompare else
                compare this.Name ttn.Name
            | _ -> -1

[<AutoOpen>]
module TypeTree = 
    type TypeTree(asmc: AssemblyCache) =
        let typeTreeCache = Dictionary<TypeDefinition, TypeTreeNode>()

        let rec buildTypeNode (t:TypeDefinition) =
            if t = null then failwith "Type cannot be null"

            Dict.tryGetValue t typeTreeCache
            |> Option.defaultWith (fun () ->
                let b = buildBaseTypeNode t
                let ifaces = buildInterfaceTypeNodes t

                let result = TypeTreeNode(asmc.GetTreeNode t.Module.Assembly, t, b, ifaces)

                typeTreeCache.Add(t, result)

                result.Base.iter(fun b -> b.Children.Add(result))
                result.Interfaces.iter(fun b -> b.Children.Add(result))

                result
            )

        and buildBaseTypeNode (t:TypeDefinition) =
            if t.BaseType = null then None else
            let bt = t.BaseType.Resolve()
            if bt = null then
                Log.Warning("Unable to resolve base type {Base:L} for type {Type:L}", t.BaseType.FullName, t.FullName)
                None
            else
                Some (buildTypeNode bt)

        and buildInterfaceTypeNodes (t:TypeDefinition) =
             t.Interfaces.map(fun i ->
                 let it = i.InterfaceType.Resolve()
                 if it = null then
                     Log.Warning("Unable to resolve interface type {Base:L} for type {Type:L}", i.InterfaceType.FullName, t.FullName)
                     None
                 else
                     Some (buildTypeNode it))
             |> Seq.choose id
             |> Seq.toArray

        do asmc.Assemblies |> Seq.collect AssemblyDefinition.allTypes |> Seq.iter (fun t -> buildTypeNode t |> ignore)

        member _.GetNode td = typeTreeCache[td]
        member _.GetNodeByType (t:Type) =
            let asmName = t.Assembly.GetName().Name
            typeTreeCache.Keys.pick(fun k -> if k.Name = t.Name && k.Module.Assembly.Name.Name = asmName then Some typeTreeCache[k] else None)
        
        member _.AllTypes : TypeTreeNode seq = upcast typeTreeCache.Values

    type TypeNodeResolver = TypeDefinition -> TypeTreeNode