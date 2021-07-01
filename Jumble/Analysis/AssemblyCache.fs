﻿namespace Jumble.Analysis

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open FSharpPlus
open Mono.Cecil
open Serilog

open Jumble

[<AutoOpen>]
module AssemblyCache = 
    type private AssemblyRefOrDef = 
    | Def of AssemblyDefinition 
    | Ref of AssemblyNameReference

    type AssemblyCache (fw:FrameworkVersion option, searchPaths) = 
        let assemblyFis = Dictionary<FilePathComparer.FileInformation, AssemblyDefinition>()
        let assemblyNames = Dictionary<string, AssemblyDefinition>(StringComparer.InvariantCultureIgnoreCase)
        
        let assemblyTreeNodes = Dictionary<AssemblyDefinition, AssemblyTreeNode>()

        let assemblyResolver = new CustomAssemblyResolver(fw, (fun anr -> Dict.tryGetValue anr.Name assemblyNames))
        let readerParameters = ReaderParameters(AssemblyResolver = assemblyResolver)

        /// Updates ModuleDefinition.Assembly for multi-module assemblies
        // https://github.com/jbevain/cecil/issues/652
        let patchAssemblyLink (ad:AssemblyDefinition) =
            ad.Modules
            |> Seq.filter (fun m -> m.Assembly = null)
            |> Seq.iter (fun m ->
                // Modules in multi-module assemblies do not have assembly set :/
                let fld = typeof<ModuleDefinition>.GetField("assembly", BindingFlags.NonPublic ||| BindingFlags.Instance)
                fld.SetValue(m, ad)    
            )
        
        let rec addAssemblyRec (assemblyDef:AssemblyDefinition) =
            if assemblyNames.TryAdd(assemblyDef.Name.Name, assemblyDef) then
                Log.Debug("Adding assembly {Name:l} ({Path:l}) to cache", assemblyDef.Name.Name, assemblyDef.MainModule.FileName)
                let fi = FilePathComparer.getFileInformation assemblyDef.MainModule.FileName
                assemblyFis.Add(fi, assemblyDef)
                assemblyResolver.AddSearchDirectoryFromModule assemblyDef.MainModule
                
                patchAssemblyLink assemblyDef
                
                let mutable treeNodeRefs = []
                let getRefs = fun () -> treeNodeRefs
                let treeNode = AssemblyTreeNode(assemblyDef, getRefs)
                assemblyTreeNodes.Add(assemblyDef, treeNode)
                
                let refs = assemblyDef.Modules
                           |> Seq.collect (fun m -> m.AssemblyReferences)
                           |> Seq.choose (fun ar ->
                                       match assemblyResolver.ResolveAssembly ar with
                                       | Some ad -> addAssemblyRec ad; Some assemblyTreeNodes.[ad]
                                       | None ->
                                           // quite often even framework dlls have missing references (e.g. netcoreapp3.1 mscorlib pointing to System.Threading.AccessControl
                                           // we can't fail here 
                                           Log.Warning("Unable to resolve assembly {Assembly:l} referenced by {Ref:l}", ar.Name, assemblyDef.Name.Name)
                                           None 
                                       )
                           |> Seq.distinct
                           |> Seq.toList
                
                treeNodeRefs <- refs
                treeNode.FinalizeRefs()
            
        do
            searchPaths |> Seq.iter assemblyResolver.AddSearchDirectory
       
        static member FromPaths (fw:FrameworkVersion option) (dlls: string list) (additionalSearchPaths: string list) =
            let asmCache = new AssemblyCache(fw, additionalSearchPaths)
            asmCache.AddDlls dlls
            asmCache
       
        member _.Assemblies with get() = assemblyNames.Values :> ICollection<AssemblyDefinition>
        
        member _.AddDll path =
            try 
                let dllFi = FilePathComparer.getFileInformation path
                if assemblyFis.ContainsKey(dllFi) = false then 
                    let m = ModuleDefinition.ReadModule(path, readerParameters)
                    if m.Assembly = null then failwithf $"File %s{path} does not contain an assembly header"
                    addAssemblyRec m.Assembly
            with :? FileNotFoundException ->
                failwithf $"Unable to add dll to assembly cache - file '%s{path}' not found"

        member this.AddDlls (paths:string list) =
            paths |> List.iter (fun p -> assemblyResolver.AddSearchDirectory (Path.GetDirectoryName p))
            paths |> List.iter (this.AddDll)

        member this.Dispose() = 
            this.Assemblies |> Seq.iter (fun a -> a.Dispose())
            assemblyFis.Clear()
            assemblyNames.Clear()
            assemblyTreeNodes.Clear()

        member _.TryGetByDllPath path =
            let fi = FilePathComparer.getFileInformation path
            Dict.tryGetValue fi assemblyFis
            
        member _.TryGetByName name = match assemblyNames.TryGetValue name with true, ad -> Some ad | _ -> None
        
        member _.GetByName name = assemblyNames.[name]
        
        member _.GetTreeNode (ad:AssemblyDefinition) =
            Dict.tryGetValue ad assemblyTreeNodes
            |> Option.defaultWith (fun () -> failwithf $"Cannot resolve %s{ad.Name.Name} from treenode cache")

        interface IDisposable with
            member this.Dispose() = this.Dispose()