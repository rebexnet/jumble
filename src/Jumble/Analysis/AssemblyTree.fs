namespace Jumble

open System
open System.Collections.Generic
open System.IO

open Mono.Cecil
open Serilog

open Jumble

module private AssemblyTreeInternals =
    type CustomAssemblyResolver (fw:FrameworkVersion option,
                                 assemblyByName:AssemblyNameReference -> AssemblyDefinition option) =
        let searchDirs = HashSet<string>()

        do
            match fw with
            | Some fw ->
                FrameworkVersion.assemblyDirs fw
                |> List.iter (fun d ->
                    Log.Debug("Adding framework directory {Dir:l} to search paths", d)
                    searchDirs.Add(d) |> ignore
                )
            | None -> ()

        member private this.ResolveAssemblyInSearchPaths (name:AssemblyNameReference) =
            this.ResolveInSearchPaths([name.Name + ".dll"; name.Name + ".exe"])
            |> Option.map (fun m -> if m.Assembly = null then None else Some m.Assembly)
            |> Option.flatten

        member private this.ResolveInSearchPaths (names:string list) : ModuleDefinition option =
            searchDirs
            |> Seq.collect (fun d -> names |> List.map (fun n -> Path.Combine(d, n)))
            |> Seq.filter (fun f -> File.Exists(f))
            |> Seq.tryPick (fun f ->
                try
                    let ad = ModuleDefinition.ReadModule(f, ReaderParameters(AssemblyResolver = this))
                    Some ad
                with
                    | :? BadImageFormatException -> None
                    | e ->
                        Log.Error("Unexpected exception: {Exception}", e)
                        None
            )

        member _.AddSearchDirectory dirName =
            searchDirs.Add(dirName) |> ignore

        member this.AddSearchDirectoryFromModule (moduleDef:ModuleDefinition) =
            let path = Path.GetDirectoryName(Path.GetFullPath(moduleDef.FileName))
            this.AddSearchDirectory path

        member this.ResolveAssembly (name:AssemblyNameReference) : AssemblyDefinition option =
            assemblyByName name
            |> Option.orElseWith (fun () -> this.ResolveAssemblyInSearchPaths name)

        interface IAssemblyResolver with
            member _.Dispose() = ()
            member this.Resolve(name: AssemblyNameReference) = Option.defaultValue null (this.ResolveAssembly name)
            member this.Resolve(name, _) = Option.defaultValue null (this.ResolveAssembly name)

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

    override _.Equals(o:obj) =
        match o with
        | :? AssemblyTreeNode as atn -> this.Assembly.MainModule.FileName = atn.Assembly.MainModule.FileName
        | _ -> false

    override _.GetHashCode() = this.Assembly.MainModule.Mvid.GetHashCode()

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? AssemblyTreeNode as atn -> compare this.Assembly.MainModule.FileName atn.Assembly.MainModule.FileName
            | _ -> -1