namespace Jumble.Export

open FSharpPlus
open System.IO
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
    /// Saves the obfuscated assembly to targetDir, optionally signing it using signingKey
    let private exportAssembly (path:string) (signingKey:SigningKey option) (asm:AssemblyDefinition) =
        Directory.CreateDirectory (Path.GetDirectoryName(path)) |> ignore

        let parameters = WriterParameters()
        match signingKey with
        | Some signingKey ->
            Log.Information("Writing {Path:l} and signing with key {Key:l}", path, signingKey)
            SigningKey.apply parameters signingKey
        | None ->
            Log.Information("Writing {Path:l}", path, signingKey)
            
        asm.MainModule.Write(path, parameters)


    let private exportAssemblyDir targetDir (signingKey:SigningKey option) (asm:AssemblyDefinition) =
        let path = Path.Combine(targetDir, Path.GetFileName(asm.MainModule.FileName)) |> Path.GetFullPath
        exportAssembly path signingKey asm

    /// Saves all obfuscated assemblies
    let export (opts:OutputOptions) (assembliesOpts:AssemblyObfuscationOptions list) =
        match opts.ExportFilter, opts.ExportTarget with
        | ModifiedOnly, DryRun -> None
        | ModifiedOnly, FlattenTo targetDir ->
            let getDestPath (a:AssemblyDefinition) = Path.Combine(targetDir, Path.GetFileName(a.MainModule.FileName)) |> Path.GetFullPath

            let exportedAssemblies = assembliesOpts |> List.filter (fun o -> o.Options.Modifiable)

            // export mapfile, fingerprints and dlls
            [
                // yield exportMapfile()
                yield! exportedAssemblies |> List.map (fun a -> async { exportAssembly (getDestPath a.Assembly) a.Options.SigningKey a.Assembly })
            ]
            |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously

            exportedAssemblies
            |> List.map (fun a -> { ExportedModulePaths.ExportedPath = getDestPath a.Assembly; OriginalPath = a.Assembly.MainModule.FileName })
            |> Some