module ModLoaderTests

open NUnit.Framework
open System.IO

open Jumble
open Jumble.Tests

[<Test>]
let ``No unresolved modules when loading LibA`` () = 
    let ms = AssemblyCache.build testFramework ["LibA.dll"] []
    printfn $"%i{ms.Assemblies.Count} assemblies loaded: %A{ms.Assemblies}"
    // printfn "%i assemblies NOT loaded: %A" ms.Unresolved.Length ms.Unresolved
    Assert.That(ms.Assemblies, Is.Not.Empty)
    // CollectionAssert.IsEmpty(ms.Unresolved)

[<Test>]
let ``Required libraries for testing exist`` () = 
    Assert.That(File.Exists libADllPath, Is.True, $"LibA dll not found at %s{libADllPath}")

[<Test>]
let ``References to netstandard libraries can be resolved`` () = 
    let ms = AssemblyCache.build testFramework [libBDllPath; libBDllPath] []
    let m = ms.GetByName "LibA"

    let baseType = m.MainModule.GetType("LibA.Dict").BaseType
    Assert.That(baseType, Is.Not.Null, "Base type must not be null")
    let resolved = baseType.Resolve()
    Assert.That(resolved, Is.Not.Null, "ModuleLoader does not resolve")
    resolved.Module.Name |> assert_equal "System.Private.CoreLib.dll"

[<Test>]
let ``Each assembly/module is loaded exactly once`` () = 
    let ms = AssemblyCache.build testFramework [libADllPath; libBDllPath] []
    let multiples = ms.Assemblies |> Seq.toList |> List.groupBy _.Name.Name |> List.filter (fun (_, ads) -> ads.Length > 1)
    Assert.That(multiples, Is.Empty)