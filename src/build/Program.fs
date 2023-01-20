// ./build pack <version>

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Build

let parentDir (p:string) = Path.GetDirectoryName(p)
let rootPath = __SOURCE_DIRECTORY__ |> parentDir |> parentDir
let outDir = rootPath @@ "out"
let buildDir = outDir @@ "bin"
let nugetDir = outDir @@ "nuget"

Environment.CurrentDirectory <- rootPath

// this is required for msbuild
//let newPathEnvVar = (Environment.environVar "PATH", Path.getFullName ".paket/") ||> sprintf "%s;%s"
//Environment.setEnvironVar "PATH" newPathEnvVar

module Exec =
    let git = ExeHelper.mktoolResult "git"
    let dotnet = ExeHelper.mktool "dotnet"
    let paket = ExeHelper.mktool "paket"


module Targets =
    let build () =
        Exec.dotnet "build -c Release src/Jumble.lib"
        Exec.dotnet $"build -c Release -o \"{buildDir}\" src/Jumble"

    let clean () =
        !! outDir ++ "**/bin" ++ "**/obj" -- "src/build/**" |> Seq.toArray |> Shell.cleanDirs

    let pack () =
        let getVersion msg =
            printf msg
            let versionString = System.Console.ReadLine()
            if String.IsNullOrWhiteSpace(versionString) then "1.0.0" else versionString

        let version = getVersion "Nuget version: "
        Exec.paket $"pack --version {version} --template \"./paket.template\" \"{nugetDir}\""
        Exec.paket $"pack --version {version} --template \"src/Jumble.Lib/paket.template\" \"{nugetDir}\""

    let zip () =
        let currentVersionTags =
            Exec.git "tag --points-at HEAD"
            |> String.splitStr Environment.NewLine
            |> List.choose (fun s -> let m = Regex.Match(s, @"^v([\d.]+)") in if m.Success then Some m.Groups[1].Value else None)

        let fileName = match currentVersionTags with
                       | [v] -> $"Jumble-v%s{v}.zip"
                       | [] -> printfn "No release tag detected"; "Jumble.zip"
                       | _ -> printfn "More than one release tag detected"; "Jumble.zip"


        let files = !! (buildDir @@ "**")
        Zip.createZip buildDir (outDir @@ fileName) "" 5 false files
        printfn $"Created zip file %s{fileName}"

    // Run NUnit tests
    let tests () = Exec.dotnet "test src/Jumble.Tests"

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
        pack()
    | [| "zip" |] ->
        clean()
        build()
        zip()
    | _ -> failwith $"Target %A{args} is not supported"

    0