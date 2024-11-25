namespace Jumble.Tests.Export

open System.IO
open Jumble.Export
open Jumble.Export.Mapfile
open Jumble.Tests.Integration
open NUnit.Framework

type CSharpExportTests() as this =
    inherit E2ETestsBase()

    [<Test>]
    member _.ClassSig() =
        let publicClass = this.Setup.Original.LibA.GetType(typeof<LibA.CA1_InheritsIA>.FullName)
        let signature = CSharpExport.typeSig publicClass { FullName = "Foo.Bar.Baz"; GenericParameters = [] }
        let expectedSignature = "public class Foo.Bar.Baz"
        Assert.That(signature, Is.EqualTo expectedSignature)

    [<Test>]
    [<Ignore("Run manually")>]
    member _.AssemblyExport() =
        let content = File.ReadAllText(Path.Combine(this.Setup.OutputDirectory, Exporter.mapFileName))
        printfn $"%s{content}"