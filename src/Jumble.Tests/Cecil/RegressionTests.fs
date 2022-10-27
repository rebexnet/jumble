namespace Jumble.Tests.Cecil

open Jumble.Tests
open NUnit.Framework

[<TestFixture>]
type RegressionTests() =
    inherit CecilTestsBase()

    // if this fails (property found), then good news, it will be much simpler to implement Lookups
    [<Test>]
    member this.``LookupToken does not return properties``() =
        let liba = this.LH.AssemblyCache.Assemblies |> Seq.find (fun a -> a.Name.Name = "LibA")
        let mdl = liba.MainModule

        // let prop = mdl.LookupToken(0x17000001) // ValGetterA, probably
        let prop = mdl.LookupToken(0x17000001) // ValGetterA, probably
        Assert.IsNull(prop)