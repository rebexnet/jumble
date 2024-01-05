// ./build pack <version>

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators

open Sajagi.Exec

let parentDir (p:string) = Path.GetDirectoryName(p)
let rootPath = __SOURCE_DIRECTORY__ |> parentDir |> parentDir
let outDir = rootPath @@ "out"
let buildDir = rootPath @@ "src/Jumble.Console/bin/Release/net6.0"
let nugetDir = outDir @@ "nuget"

Environment.CurrentDirectory <- rootPath

let git = where "git"
let dotnet = where "dotnet"
let paket = rootPath @@ ".paket" @@ "paket.exe"

module Targets =
    let build () =
        exec dotnet "build -c Release src/Jumble.Console"

        // remove localization assemblies
        Directory.EnumerateDirectories(buildDir) |> Seq.iter (fun d -> Directory.Delete(d, true))

    let clean () =
        !! outDir ++ "**/bin" ++ "**/obj" -- "src/build/**" |> Seq.toArray |> Shell.cleanDirs

    let pack (version: string option) =
        let getVersion msg =
            printf msg
            let versionString = System.Console.ReadLine()
            if String.IsNullOrWhiteSpace(versionString) then "1.0.0" else versionString

        let version = version |> Option.defaultWith (fun () -> getVersion "Nuget version: ")

        exec paket $"pack --version {version} --template \"src/Jumble.Console/paket.template\" \"{nugetDir}\""
        exec paket $"pack --version {version} --template \"src/Jumble/paket.template\" \"{nugetDir}\""

    let zip () =
        let currentVersionTags =
            execWithOutput git "tag --points-at HEAD"
            |> String.splitStr Environment.NewLine
            |> List.choose (fun s -> let m = Regex.Match(s, @"^v([\d.]+)") in if m.Success then Some m.Groups[1].Value else None)

        let fileName = match currentVersionTags with
                       | [v] -> $"Jumble-v%s{v}.zip"
                       | [] -> printfn "No release tag detected"; "Jumble.zip"
                       | _ -> printfn "More than one release tag detected"; "Jumble.zip"


        let files = !! (buildDir @@ "**")
        Directory.CreateDirectory(outDir) |> ignore
        Zip.createZip buildDir (outDir @@ fileName) "" 5 false files
        printfn $"Created zip file %s{fileName}"

    // Run NUnit tests
    let tests () = exec dotnet "test src/Jumble.Tests"

open Targets;

[<EntryPoint>]
let main (args: string[]) =

    match args with
    | [| "all" |] ->
        clean()
        build()
        tests()
        zip()
    | [| "build" |] ->
        build()
    | [| "clean" |] ->
        clean()
    | [| "tests" |] ->
        tests()
    | [| "pack" |] ->
        pack None
    | [| "pack"; version |] ->
        pack (Some version)
    | [| "zip" |] ->
        clean()
        build()
        zip()
    | _ -> failwith $"Target %A{args} is not supported"

    0