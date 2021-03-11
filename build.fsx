open System

#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open System.Text

module Path =
    let rec split (s:string) =
        let s = s.TrimEnd('/', '\\')
        let parentDir = Path.getDirectory s
        (parentDir, s.[parentDir.Length + 1..])

let buildRoot = "./build"
let buildDir = "./build/bin"
let nugetDir = "./build/nuget"

let ensureExitCode (p:ProcessResult) = if p.ExitCode <> 0 then failwithf "Process exited with code %i" p.ExitCode

Target.create "Build" (fun _ ->
    DotNet.build id "./Jumble.lib"
    DotNet.build (fun opts -> { opts with OutputPath = Some buildDir }) "./Jumble"
)

// Clean all directories
Target.create "Clean" (fun _ -> !!buildRoot ++ "**/bin" ++ "**/obj" |> Seq.toArray |> Shell.cleanDirs)

Target.create "Pack" (fun _ ->
    let getVersion msg =
        printf msg
        let versionString = System.Console.ReadLine()
        if (String.IsNullOrWhiteSpace(versionString)) then "1.0.0" else versionString
     
    let version = getVersion "Nuget version: "
    Paket.pack (fun opts -> { opts with Version = version; OutputPath = nugetDir; TemplateFile = "./paket.template" })
    Paket.pack (fun opts -> { opts with Version = version; OutputPath = nugetDir; TemplateFile = "./Jumble.Lib/paket.template" })
)

// Run NUnit tests
Target.create "Tests" (fun _ -> DotNet.test id "./Jumble.Tests")

// specify dependencies and order
let (<==*) x y =
    y |> List.iter (fun yitem -> yitem ==> x |> ignore)
    y |> List.pairwise |> List.iter (fun (first, second) -> first ?=> second |> ignore)

let nops x = x |> Seq.iter (fun t -> Target.create t ignore)

nops ["All"]
"All" <==* ["Build"; "Tests"; "Pack"]
"Build" <==* ["Clean"]


Target.runOrList()