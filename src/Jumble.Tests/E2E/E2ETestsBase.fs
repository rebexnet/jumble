namespace Jumble.Tests.Integration

open System
open System.IO
open Jumble
open Jumble.Export
open Jumble.Rename
open Jumble.Tests
open Mono.Cecil
open NUnit.Framework

type ModulePair = {
    LibA: ModuleDefinition
    LibB: ModuleDefinition
    LibC_PrivateOnly: ModuleDefinition
    Suffix: string
 }

type E2ESetup =
    {
        OriginalLibA: ModuleDefinition
        OriginalLibB: ModuleDefinition
        OriginalLibC: ModuleDefinition
        ObfuscatedLibA: ModuleDefinition
        ObfuscatedLibB: ModuleDefinition
        ObfuscatedLibC: ModuleDefinition
        OutputDirectory: string
        Map: RenameMap
    }
    member this.Original = {
               LibA = this.OriginalLibA
               LibB = this.OriginalLibB
               LibC_PrivateOnly = this.OriginalLibC
               Suffix = ""
    }
    
    member this.Obfuscated = {
        LibA = this.ObfuscatedLibA
        LibB = this.ObfuscatedLibB
        LibC_PrivateOnly = this.ObfuscatedLibC
        Suffix = NameGenerators.testNameSuffix
    }

    interface IDisposable with
        member this.Dispose() =
            this.OriginalLibA.Dispose()
            this.OriginalLibB.Dispose()
            this.OriginalLibC.Dispose()
            this.ObfuscatedLibA.Dispose()
            this.ObfuscatedLibB.Dispose()
            this.ObfuscatedLibC.Dispose()

type E2ETestsBase() =
    static let mutable (setup: E2ESetup option) = None
    static let mutable (libABSigningKey: SigningKey option) = None

    static let obfuscateTestLibs() =
        let outputPath = Path.Combine(Path.GetTempPath(), "jumble-e2etests")
        printfn $"Output path is %s{outputPath}"
        if Directory.Exists(outputPath) then Directory.Delete(outputPath, true)
        Directory.CreateDirectory(outputPath) |> ignore

        let map = obfuscate (fun p -> { p with
                                                        Dlls = [
                                                         DllObfuscationOptions.fromDllPathWithKey PrivateAndPublic libADllPath [] libABSigningKey.Value
                                                         DllObfuscationOptions.fromDllPathWithKey PrivateAndPublic libBDllPath [] libABSigningKey.Value
                                                         DllObfuscationOptions.fromDllPath PrivateOnly libCDllPath []
                                                        ]
                                                        Framework = testFramework
                                                        MethodNameGenerator = NameGenerators.NameGenTest
                                                        TypeNameGenerator = NameGenerators.NameGenTest
                                                        Output = { p.Output with
                                                                     ExportTarget = FlattenTo outputPath
                                                                     ExportFilter = ModifiedOnly }

        })

        {
            OriginalLibA = ModuleDefinition.ReadModule(libADllPath)
            OriginalLibB = ModuleDefinition.ReadModule(libBDllPath)
            OriginalLibC = ModuleDefinition.ReadModule(libCDllPath)
            ObfuscatedLibA = ModuleDefinition.ReadModule(Path.Combine(outputPath, libAAssemblyName + ".dll"))
            ObfuscatedLibB = ModuleDefinition.ReadModule(Path.Combine(outputPath, libBAssemblyName + ".dll"))
            ObfuscatedLibC = ModuleDefinition.ReadModule(Path.Combine(outputPath, libCAssemblyName + ".dll"))
            Map = map
            OutputDirectory = outputPath
        }

    [<OneTimeSetUp>]
    member _.OneTimeSetup() =
        if setup.IsNone then
            libABSigningKey <- Some <| SigningKey.fromSnkFile @"sign.snk"
            setup <- Some <| obfuscateTestLibs()

    member _.Setup with get () = setup.Value

    member _.LibABSigningKey with get () = libABSigningKey.Value
    
    member this.FindTypeByDescriptionAttribute (md:ModuleDefinition) (desc:string) =
        let descAttrValue (t:TypeDefinition) =
            t.CustomAttributes
            |> Seq.tryFind (fun a -> a.AttributeType.FullName = "System.ComponentModel.DescriptionAttribute")
            |> Option.map (fun a -> a.ConstructorArguments[0].Value :?> string)
        md.Types |> Seq.find (fun t -> descAttrValue t = Some desc)

[<TestFixture>]
type E2EFixtureTest() =
    inherit E2ETestsBase()

    [<Test>]
    member this.OneTimeFixtureWorks() =
        Assert.IsNotNull(this.Setup.OriginalLibA)
        Assert.IsNotNull(this.Setup.OriginalLibB)
        Assert.IsNotNull(this.Setup.OriginalLibC)
        Assert.IsNotNull(this.Setup.ObfuscatedLibA)
        Assert.IsNotNull(this.Setup.ObfuscatedLibB)
        Assert.IsNotNull(this.Setup.ObfuscatedLibC)

[<TestFixture>]
type E2EManualFixtureTest() =
    // [<Ignore("Manual run only")>]
    [<Test>]
    member _.RunE2EFixture() =
        let t = E2EFixtureTest()
        t.OneTimeSetup()