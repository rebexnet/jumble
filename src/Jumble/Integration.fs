namespace Jumble

open System.IO
open Jumble.Rename.Exclusion
open Serilog

[<AutoOpen>]
module Integration =
    open FSharp.Core.Fluent
    open Mono.Cecil

    open Jumble.Analysis
    open Jumble.Export
    open Jumble.Rename
    open Jumble.Rename.RenameFilter
    open Jumble.Rename.NameGenerators

    let defaultFramework = FrameworkVersion.createS NETFramework "4.5"
    
    type ObfuscateParams =
        { Dlls: DllObfuscationOptions list
          Framework: FrameworkVersion option
          LogDir: string option
          Output: OutputOptions
          GenericParameterNameGenerator: NameGeneratorType
          MethodNameGenerator: NameGeneratorType
          ParameterNameGenerator: NameGeneratorType
          SearchPaths: string list
          TypeNameGenerator: NameGeneratorType
          RenameFilters: ExclusionFilterType list }

    let private defaultParams =
        { Dlls = []
          Framework = Some defaultFramework
          LogDir = None
          Output =
              { ExportFilter = ModifiedOnly
                ExportTarget = DryRun }
          GenericParameterNameGenerator = NameGenOrder
          MethodNameGenerator = NameGenDefault Seed.RandomSeed
          ParameterNameGenerator = NameGenOrder
          SearchPaths = []
          TypeNameGenerator = NameGenDefault Seed.RandomSeed
          RenameFilters = [] }
    
    let obfuscate (f: ObfuscateParams -> ObfuscateParams) =
        let opts = f defaultParams
        let dllPaths = opts.Dlls.map (fun dll -> dll.DllPath)
        Log.Information("Loading assemblies...")
        let searchPaths = opts.SearchPaths
                          |> List.append (dllPaths |> List.map Path.GetDirectoryName)
                          |> List.distinct
        
        let asmCache = new AssemblyCache(opts.Framework, searchPaths)
        asmCache.AddDlls dllPaths
        
        Log.Information("Loaded {Asm} assemblies", asmCache.Assemblies.Count)

        let typeTree = TypeTree(asmCache)

        let findOptToAsm (asm: AssemblyDefinition): AssemblyObfuscationOptions =
            match opts.Dlls |> List.tryFind (fun o -> comparePaths o.DllPath asm.MainModule.FileName) with
            | None ->
                let opts = {
                    DllObfuscationOptions.DllPath = asm.MainModule.FileName
                    ObfuscationLevel = Untouchable
                    ExceptFilters = []
                    SigningKey = None
                }
                
                { Assembly = asm
                  Options = opts }
            | Some o ->
                { Assembly = asm
                  Options = o }

        let assembliesOpts =
            asmCache.Assemblies
            |> Seq.map findOptToAsm
            |> Seq.toList


        let rsvlr = Resolvers.createSafeMemoized()

        Log.Information("Running code analysis...")
        // The analysis is used for these purposes only:
        // - to make sure all references to renamed types and members are also renamed
        // - detection of EnumToString conversion
        // Therefore .. we do NOT need to analyze code which does NOT reference obfuscated assemblies
        let caResult =
            assembliesOpts
            |> List.choose (fun o -> if o.Options.Modifiable then Some o.Assembly else None)
            |> List.collect (fun a -> [yield a; yield! asmCache.GetTreeNode(a).ReferencedByRec |> Seq.map (fun n -> n.Assembly)])
            |> List.distinct
            |> CodeAnalysis.analyseAssemblies rsvlr

        Log.Debug "Creating exclusion filters..."
        let filters = List.append opts.RenameFilters (ExclusionFilter.buildFilters typeTree.GetNode)

        // We go through all the code and find types and members which should not be renamed
        let exclusions = assembliesOpts
                         |> Seq.collect (fun asm -> ExclusionFilter.findExclusions typeTree.GetNode filters asm)
                         |> Seq.append (caResult.EnumToStringConversion |> Array.map (fun c -> ExclusionScopeAndReason.createType c AppliesToAllMembers ExclusionReason.StringConversion))
                         |> Exclusions.create

        // todo: it's actually on assembly level
        if (opts.Dlls |> List.exists(fun o -> o.ExceptFilters = [FltEnumToString])) then
            exclusions.siftEnumToString()

        Log.Debug "Filter created"
        let memberGroups = Grouping.groupMembers typeTree
        let memberFilterResult = RenameFilter.filterGroups assembliesOpts exclusions memberGroups

        match opts.LogDir with
        | Some logDir ->
            let filterResult =
                { FilteredGroups = memberFilterResult
                  Exclusions = exclusions }
            Jumble.Html.HtmlExport.grouping filterResult (System.IO.Path.Combine(logDir, "report.html"))
        | None -> ()

        Log.Debug("Building type reference cache...")

        let membersToRename =
            memberFilterResult
            |> Array.map (fun g ->
                g
                |> Seq.map (fun r -> r.Member)
                |> Seq.toArray)

        let typeToAsmObfLevel (t: TypeDefinition) =
            (assembliesOpts |> List.find (fun a -> t.Module.Assembly = a.Assembly)).Options.ObfuscationLevel

        let typesToRename = RenameFilter.filterTypes typeToAsmObfLevel exclusions typeTree.AllTypes

        let methodNameGen = NameGenerators.buildMethodGen opts.MethodNameGenerator asmCache.Assemblies
        let typeNameGen = NameGenerators.buildTypeGen opts.TypeNameGenerator asmCache.Assemblies
        let paramNameGen = NameGenerators.buildParameterGen opts.ParameterNameGenerator
        let genParNameGen = NameGenerators.buildGenenericParameterGen opts.GenericParameterNameGenerator

        // actually perform renaming
        Log.Information "Rename phase"

        // plans
        Log.Debug("Creating rename plans...")
        let memberRenamePlans = MemberRename.createRenamePlans methodNameGen paramNameGen membersToRename
        let typeRenamePlans = TypeRename.createRenamePlans typeNameGen genParNameGen typesToRename

        let renameResult =
            { TypeRenamePlans = typeRenamePlans
              MemberRenamePlans = memberRenamePlans }

        let structuredResult = RenameMap.fromRenameResult renameResult

        // rename
        MemberRename.renameMembers caResult.Lookups.MemberLookup memberRenamePlans
        TypeRename.renameTypes caResult.Lookups.TypeLookup typeRenamePlans

        // patch references
        Log.Information("Patching assembly public keys...")
        assembliesOpts
        |> List.filter (fun a -> a.Options.Modifiable)
        |> List.iter (fun a -> ReferencePatch.updateAssemblyPublicKey a.Assembly a.Options.SigningKey)

        Log.Information("Patching assembly refs and friend refs public keys...")
        assembliesOpts
        |> List.filter (fun a -> a.Options.Modifiable)
        |> List.iter (fun a ->
            ReferencePatch.patchAssemblyRefs a.Assembly asmCache.GetByName
            ReferencePatch.patchFriendAssemblyRefs a.Assembly asmCache.TryGetByName
        )

        // export
        Log.Information "Exporting assemblies..."

        Exporter.export opts.Output assembliesOpts structuredResult |> ignore

        Log.Information "Done!"
        structuredResult