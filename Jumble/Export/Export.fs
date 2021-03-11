namespace Jumble.Export

open System.IO
open System.Security.Cryptography.X509Certificates
open Jumble
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
        |> Seq.iter (fun ar -> ar.PublicKey <- (res ar.Name).Name.PublicKey)

    /// Patches public keys in [InternalsVisibleTo(...)] attributes
    let patchFriendAssemblyRefs (asm:AssemblyDefinition) (res:string -> AssemblyDefinition option) =

        asm.CustomAttributes
        |> Seq.filter (fun attr -> attr.AttributeType.FullName = "System.Runtime.CompilerServices.InternalsVisibleToAttribute")
        |> Seq.iter (fun attr ->
            let ref = attr.ConstructorArguments.[0].Value :?> string

            let currentAsmRefName = match ref.IndexOf(',') with -1 -> ref | i -> ref.Substring(0, i)
            match res currentAsmRefName with
            | None -> ()
            | Some asmRef ->
                let newAsmRefName = match asmRef.Name.PublicKey with [||] -> asmRef.Name.Name | pk -> sprintf "%s, PublicKey=%s" asmRef.Name.Name (SigningKey.publicKeyString pk)

                let stringTypeRef = attr.ConstructorArguments.[0].Type
                if attr.ConstructorArguments.Count <> 1 then failwith "Expected only 1 argument for InternalsVisibleToAttribute"
                attr.ConstructorArguments.RemoveAt(0)
                attr.ConstructorArguments.Add(CustomAttributeArgument(stringTypeRef, newAsmRefName))
        )


module Exporter =
    /// Saves the obfuscated assembly to targetDir, optionally signing it using signingKey
    let private exportAssembly targetDir (signingKey:SigningKey option) (asm:AssemblyDefinition) = 
        Directory.CreateDirectory targetDir |> ignore
    
        let path = Path.Combine(targetDir, Path.GetFileName(asm.MainModule.FileName)) |> Path.GetFullPath
        
        let parameters = WriterParameters()
        match signingKey with
        | Some signingKey ->
            Log.Information("Writing {Path:l} and signing with key {Key:l}", path, signingKey)
            SigningKey.apply parameters signingKey
        | None ->
            Log.Information("Writing {Path:l}", path, signingKey)
            
        asm.MainModule.Write(path, parameters)
        path

    /// Saves all obfuscated assemblies
    let export (opts:OutputOptions) (assembliesOpts:AssemblyObfuscationOptions list) (renameResult:RenameMap) =
        match opts.ExportFilter, opts.ExportTarget with 
        | _, DryRun -> { ExportResult.Dlls = [] }
        | ModifiedOnly, FlattenTo targetDir -> 
            let mapFileContent = MapFile.createMapFile renameResult
            Directory.CreateDirectory(targetDir) |> ignore
            let mapFileName = Path.Combine(targetDir, "mapfile.xml")
            File.WriteAllBytes(mapFileName, mapFileContent)
            
            let paths = assembliesOpts 
                        |> List.filter (fun opt -> opt.Options.Modifiable)
                        |> List.map (fun opt ->
                            let signingKey = opt.Options.SigningKey
                            let path = exportAssembly targetDir signingKey opt.Assembly
                            
                            AssemblyFingerprint.saveFingerprint targetDir opt.Assembly
                            { ExportedModulePaths.ExportedPath = opt.Assembly.MainModule.FileName; OriginalPath = path})
            { ExportResult.Dlls = paths }