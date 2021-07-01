﻿namespace Jumble

open FSharp.Core.Fluent
open System.Collections.Generic

open FSharpPlus
open Jumble
open Mono.Cecil
open Serilog
open System

type TypeTreeNode (t:TypeDefinition, b: TypeTreeNode option, ifaces: TypeTreeNode[]) =
    let ancestors = match b with None -> [] | Some bt -> bt :: bt.Ancestors
    let children = ResizeArray<TypeTreeNode>()
    let descendants = lazy(Seq.append children (children |> Seq.collect (fun t -> t.Descendants)) |> Seq.distinct |> Seq.toList)
    let members = TypeDefinition.members t |> Seq.toList
    member _.Ancestors = ancestors
    member _.TypeDefinition = t
    member _.Base = b
    member _.Interfaces = ifaces
    member _.Children = children
    member _.Members = members
    override x.Equals obj = Object.ReferenceEquals(x, obj)
    override x.GetHashCode () = x.TypeDefinition.GetHashCode()

    member this.AncestorsAndSelf = this::this.Ancestors

    member _.Descendants with get() = descendants.Value

    member _.FindMember name = members |> Seq.filter (fun m -> m.Name = name) |> Seq.exactlyOne

    override this.ToString() = this.TypeDefinition.FullName

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? TypeTreeNode as ttn ->
                if this = ttn then 0 else compare this.TypeDefinition.FullName ttn.TypeDefinition.FullName
            | _ -> -1

[<AutoOpen>]
module TypeTree = 
    type TypeTree(asm: AssemblyCache) = 
        let typeTreeCache = Dictionary<TypeDefinition, TypeTreeNode>()
        
        let rec buildTypeNode (t:TypeDefinition) =
            if t = null then failwith "Type cannot be null"
            
            Dict.tryGetValue t typeTreeCache
            |> Option.defaultWith (fun () ->  
                let b = buildBaseTypeNode t
                let ifaces = buildInterfaceTypeNodes t
                    
                let result = TypeTreeNode(t, b, ifaces)
                            
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
                
        do asm.Assemblies |> Seq.collect AssemblyDefinition.allTypes |> Seq.iter (fun t -> buildTypeNode t |> ignore)

        member _.GetNode td = typeTreeCache.[td]
        member _.GetNodeByType (t:Type) =
            let asmName = t.Assembly.GetName().Name
            typeTreeCache.Keys.pick(fun k -> if k.Name = t.Name && k.Module.Assembly.Name.Name = asmName then Some typeTreeCache.[k] else None)
        
        member _.AllTypes : TypeTreeNode seq = upcast typeTreeCache.Values

    type TypeNodeResolver = TypeDefinition -> TypeTreeNode