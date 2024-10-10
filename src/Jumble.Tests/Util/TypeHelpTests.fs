namespace Jumble.Tests

open NUnit.Framework

module TypeHelpTests =
    open Jumble.Analysis.TypeSearch
    
    [<Test>]
    let ``method finds a method when returning delegate``() = 
        let m = method<string, _> <@ fun x -> x.Clone @>
        Assert.That(m, Is.Not.Null)
        Assert.That(m.Name, Is.EqualTo "Clone")

    [<Test>]
    let ``method finds a method when calling method``() = 
        let m = method<string, _> <@ fun x -> x.CopyTo(0, null, 0, 0) @>
        Assert.That(m, Is.Not.Null)
        Assert.That(m.Name, Is.EqualTo "CopyTo")