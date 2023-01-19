namespace Jumble.Export

open FSharpPlus
open System.IO
open Jumble
open Jumble.Analysis
open Jumble.Rename
open Mono.Cecil
open Serilog

module ReferencePatch =
    /// Sets public key on the given assembly. This does NOT actually sign the assembly.
    let updateAssemblyPublicKey (asm:AssemblyDefinition) (key: SigningKey option) =
        match key with
        | None ->
            if asm.Name.HasPublicKey then Log.Warning("Removing strong name from {assembly:l} as no signing key is specified", asm.Name.Name)
            asm.Name.PublicKey <- [||]
        | Some key ->
            asm.Name.PublicKey <- key.PublicKeyBlob

    /// Patches public keys in assembly references
    let patchAssemblyRefs (asm:AssemblyDefinition) (res:string -> AssemblyDefinition) =
        asm.MainModule.AssemblyReferences
        |> Seq.iter (fun ar ->
            // Use hash (token) when former public key was also represented by hash only
            // .NET CF has an issue with AssemblyRefs with full public keys so try to keep the original type (token of full) 

            let hasToken = Array.isNotNullOrEmpty ar.PublicKeyToken
            let hasKey = Array.isNotNullOrEmpty ar.PublicKey

            if hasToken && hasKey = false then
                ar.PublicKeyToken <- (res ar.Name).Name.PublicKeyToken
            else
                ar.PublicKey <- (res ar.Name).Name.PublicKey
        )

    /// Patches public keys in [InternalsVisibleTo(...)] attributes
    let patchFriendAssemblyRefs (asm:AssemblyDefinition) (res:string -> AssemblyDefinition option) =

        asm.CustomAttributes
        |> Seq.filter (fun attr -> attr.AttributeType.FullName = "System.Runtime.CompilerServices.InternalsVisibleToAttribute")
        |> Seq.iter (fun attr ->
            let ref = attr.ConstructorArguments[0].Value :?> string

            let currentAsmRefName = match ref.IndexOf(',') with -1 -> ref | i -> ref.Substring(0, i)
            match res currentAsmRefName with
            | None -> ()
            | Some asmRef ->
                let newAsmRefName = match asmRef.Name.PublicKey with [||] -> asmRef.Name.Name | pk -> $"%s{asmRef.Name.Name}, PublicKey=%s{SigningKey.publicKeyString pk}"

                let stringTypeRef = attr.ConstructorArguments[0].Type
                if attr.ConstructorArguments.Count <> 1 then failwith "Expected only 1 argument for InternalsVisibleToAttribute"
                attr.ConstructorArguments.RemoveAt(0)
                attr.ConstructorArguments.Add(CustomAttributeArgument(stringTypeRef, newAsmRefName))
        )

module Exporter =
    let mapFileName = "mapfile.cs"

    // search fast for type rename plans using MemberID
    type TypeRenamePlans (plans: TypeRenamePlan[]) =
        let cache =
            plans
            |> Array.groupBy (fun p -> p.TypeID.MVID)
            |> Array.map (fun (mvid, ps) -> (mvid, (ps |> Array.map (fun p -> (p.TypeID.MemberToken.ToUInt32(), p)) |> Map.ofArray)))
            |> Map.ofArray

        member this.tryFind (typeId:MemberID) : TypeRenamePlan option =
            cache
            |> Map.tryFind typeId.MVID
            |> Option.bind (fun bt -> bt |> Map.tryFind (typeId.MemberToken.ToUInt32()))

        member this.byModule (m:MVID) =
            cache
            |> Map.tryFind m
            |> Option.map (fun m -> m.Values)
            |> Option.defaultValue Array.empty

    /// Saves the obfuscated assembly to targetDir, optionally signing it using signingKey
    let private exportAssembly (targetDir: string) (assembly:AssemblyDefinition) signingKey =
        let destPath = Path.Combine(targetDir, Path.GetFileName(assembly.MainModule.FileName)) |> Path.GetFullPath

        Directory.CreateDirectory (Path.GetDirectoryName(destPath)) |> ignore

        let parameters = WriterParameters()
        match signingKey with
        | Some signingKey ->
            Log.Information("Writing {Path:l} and signing with key {Key:l}", destPath, signingKey)
            SigningKey.apply parameters signingKey
        | None ->
            Log.Information("Writing {Path:l}", destPath, signingKey)

        assembly.MainModule.Write(destPath, parameters)
        { ExportedModulePaths.ExportedPath = destPath; OriginalPath = assembly.MainModule.FileName }

    /// After calling this function ModuleDefinition and AssemblyDefinition objects are in tampered state and should no longer be used
    let applyAndSave (outputDirectory:string) (lookups:Lookups) (memberRenamePlans, typeRenamePlans) (asmCache:AssemblyCache) (assembliesOpts:AssemblyObfuscationOptions list) =
        // mapfile
        // todo: move elsewhere until ---xxxx---
        let memberRenamePlansByModule = memberRenamePlans |> Array.groupBy (fun p -> p.MemberID.MVID)
        let typeRenamePlanCache = TypeRenamePlans(typeRenamePlans)
        let modulePlans = assembliesOpts
                          |> List.filter (fun a -> a.Options.Modifiable)
                          |> List.map (fun a -> {
                              ModuleRenamePlan.MVID = a.Assembly.MainModule.Mvid
                              TypeRenamePlans = typeRenamePlanCache.byModule a.Assembly.MainModule.Mvid
                              MemberRenamePlans = memberRenamePlansByModule
                                                |> Array.tryPick (fun (mvid, plans) -> if mvid = a.Assembly.MainModule.Mvid then Some plans else None)
                                                |> Option.defaultValue [||]
                          })

        Log.Debug("Module-level plans created");
        Log.Debug("Writing mapfile...")
        let mapfile = Path.Combine(outputDirectory, mapFileName)
        use fs = File.OpenWrite(mapfile)
        use writer = new StreamWriter(fs)

        modulePlans
        |> List.iter (fun p ->
            let moduleDefinition = assembliesOpts
                                   |> Seq.map (fun o -> o.Assembly.MainModule)
                                   |> Seq.find (fun m -> m.Mvid = p.MVID)

            let renameLookup (td:TypeDefinition) : TypeRenamePlan option =
                let typeId = MemberID.fromDefinition td
                typeRenamePlanCache.tryFind typeId

            timeThisSeconds "Exported mapfile for {Module}" [|moduleDefinition.Name|] (fun () -> Mapfile.exportCSharp writer renameLookup p moduleDefinition)
        )
        Log.Debug("Mapfile written to {Mapfile:l}", mapfile)

        // ---xxxx---

        // rename
        Log.Debug("Renaming type members...")
        MemberRename.renameMembers lookups.MemberRefLookup lookups.MemberIDLookup memberRenamePlans
        Log.Debug("{Num} type members renamed", memberRenamePlans.Length)
        Log.Debug("Renaming types...")
        TypeRename.renameTypes lookups.TypeRefLookup lookups.TypeIDLookup typeRenamePlans
        Log.Debug("{Num} types renamed", typeRenamePlans.Length)

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

        Log.Information "Exporting assemblies..."

        // member tokens become unusable after export
        // that's why we need to create mapfile (using token lookups) BEFORE exporting assemblies
        // also, since tokens are recomputed after saving (and not refreshed in ModuleDefinitions),
        // there is no easy (reliable) way (?) to map original tokens to new tokens.
        assembliesOpts
        |> List.filter (fun a -> a.Options.Modifiable)
        |> List.iter (fun a -> exportAssembly outputDirectory a.Assembly a.Options.SigningKey |> ignore)