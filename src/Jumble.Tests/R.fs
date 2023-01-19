namespace Jumble.Tests

open Jumble
open Microsoft.FSharp.Core

[<AutoOpen>]
module R =

    open System
    open System.IO
    open NUnit.Framework

    let libAAssemblyName = "LibA"
    let libBAssemblyName = "LibB"
    let libCAssemblyName = "LibC"

    let libADllPath = Path.GetFullPath "LibA.dll"
    let libBDllPath = Path.GetFullPath "LibB.dll"
    let libCDllPath = Path.GetFullPath "LibC.dll"

    let testFramework = FrameworkVersion.parse "netcoreapp3.1" |> Some

    [<Literal>]
    let ignNotMatchingOnParameterTypes = "Not matching on parameter types"

    type LoaderHelper () =
        let loaded = AssemblyCache.build testFramework [libADllPath; libBDllPath] []

        member _.AssemblyCache = loaded

        member _.FindTypeDef (t:Type) =
            let asm = loaded.Assemblies |> Seq.find(fun a -> a.Name.Name = t.Assembly.GetName().Name)
            asm.MainModule.GetType(t.FullName)

        member this.FindMethodDefs (t:Type) (m:string)=
            let t = this.FindTypeDef t
            t.Methods |> Seq.filter(fun me -> me.Name = m) |> Seq.toList

        member _.Dispose() =
            loaded.Dispose()

        interface IDisposable with
            member this.Dispose() = this.Dispose()

    let mutable private lh : LoaderHelper option = None

    let loadHelper() =
        lh <- Some <| Option.defaultWith (fun () -> new LoaderHelper()) lh
        lh.Value

    [<TestFixture>]
    [<AbstractClass>]
    type CecilTestsBase () =
        let lh = loadHelper()
        member _.LH = lh

    [<AbstractClass>]
    type JumbleTestsBaseWithTypeTree () as this =
        inherit CecilTestsBase()

        member val Tree = TypeTree(this.LH.AssemblyCache)

    let inline assert_notnull x = Assert.NotNull(x)
    let inline assert_none x = Assert.IsTrue(Option.isNone x)