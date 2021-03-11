namespace Jumble.Analysis

open FSharp.Core.Fluent
open System.Collections.Generic

open FSharpPlus
open Jumble
open Jumble.Utils
open Mono.Cecil
open Serilog
open System

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
                    
                let result = TypeTreeNode(asm.GetTreeNode t.Module.Assembly, t, b, ifaces)
                            
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

        member __.GetNode td = typeTreeCache.[td]
        member __.GetNodeByType (t:Type) = 
            let asmName = t.Assembly.GetName().Name
            typeTreeCache.Keys.pick(fun k -> if k.Name = t.Name && k.Module.Assembly.Name.Name = asmName then Some typeTreeCache.[k] else None)
        
        member __.AllTypes : TypeTreeNode seq = upcast typeTreeCache.Values
        member __.FindType assembly typeName = 
            typeTreeCache.Keys.pick(fun k -> if k.Module.Assembly.Name.Name = assembly && k.FullName = typeName then Some typeTreeCache.[k] else None)
    
    type TypeNodeResolver = TypeDefinition -> TypeTreeNode