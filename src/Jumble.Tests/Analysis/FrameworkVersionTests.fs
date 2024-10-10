namespace Jumble.Tests.Analysis

open NUnit.Framework

open Jumble
open NUnit.Framework.Legacy

[<TestFixture>]
type FrameworkVersionTests() =
    
    [<TestCaseSource("CanParseSource")>]
    [<Test>]
    member _.CanParse(s, expFam, expVer:string) =
        let actual = FrameworkVersion.parse s
        Assert.That(actual.Family, Is.EqualTo expFam)
        Assert.That(actual.Version, Is.EqualTo (Version.parse expVer))

    static member private CanParseSource() =
        let mk exp fam ver = TestCaseData(exp, fam, ver).SetName $"CanParse - %s{exp}"
        [
            mk "net45" NETFramework "4.5"
            mk "net4" NETFramework "4.0"
            mk "net4.5" NETFramework "4.5"
            mk "net5" NET "5.0"
            mk "net6" NET "6.0"
            mk "net8" NET "8.0"
            mk "netcoreapp5.0" NET "5.0"
            mk "netcoreapp2.1" NETCore "2.1"
            mk "netstandard2.3" NETStandard "2.3"
        ]
        
    [<Test>]
    [<TestCaseSource("CanLocateAssemblyDirsSource")>]
    member _.CanLocateAssemblyDirs(fw) =
        Assert.That(FrameworkVersion.assemblyDirs fw, Is.Not.Empty)

    static member private CanLocateAssemblyDirsSource() =
        ["net5"; "net4.5"; "netcoreapp2.1"; "netstandard2.1"]
        |> List.map FrameworkVersion.parse