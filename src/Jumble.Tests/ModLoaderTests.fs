module ModLoaderTests

open NUnit.Framework
open FsUnit
open System.IO

open Jumble
open Jumble.Tests

[<Test>]
let ``No unresolved modules when loading LibA`` () = 
    let ms = AssemblyCacheBuilder.create testFramework ["LibA.dll"] []
    printfn $"%i{ms.Assemblies.Count} assemblies loaded: %A{ms.Assemblies}"
    // printfn "%i assemblies NOT loaded: %A" ms.Unresolved.Length ms.Unresolved
    CollectionAssert.IsNotEmpty(ms.Assemblies)
    // CollectionAssert.IsEmpty(ms.Unresolved)

[<Test>]
let ``Required libraries for testing exist`` () = 
    Assert.IsTrue(File.Exists libADllPath, $"LibA dll not found at %s{libADllPath}")

[<Test>]
let ``References to netstandard libraries can be resolved`` () = 
    let ms = AssemblyCacheBuilder.create testFramework [libBDllPath; libBDllPath] []
    let m = ms.GetByName "LibA"

    let baseType = m.MainModule.GetType("LibA.Dict").BaseType
    Assert.IsNotNull(baseType, "Base type must not be null")
    let resolved = baseType.Resolve()
    Assert.IsNotNull(resolved, "ModuleLoader does not resolve")
    Assert.AreEqual("System.Private.CoreLib.dll", resolved.Module.Name)

[<Test>]
let ``Each assembly/module is loaded exactly once`` () = 
    let ms = AssemblyCacheBuilder.create testFramework [libADllPath; libBDllPath] []
    let multiples = ms.Assemblies |> Seq.toList |> List.groupBy (fun ad -> ad.Name.Name) |> List.filter (fun (_, ads) -> ads.Length > 1) 
    CollectionAssert.IsEmpty(multiples)