namespace Jumble.Tests

module TypeHelpTests = 
    open NUnit.Framework
    open Jumble.Analysis.TypeSearch
    
    [<Test>]
    let ``method finds a method when returning delegate``() = 
        let m = method<string, _> (<@ fun x -> x.Clone @>)
        Assert.IsNotNull(m)
        Assert.AreEqual("Clone", m.Name)
    
    [<Test>]
    let ``method finds a method when calling method``() = 
        let m = method<string, _> (<@ fun x -> x.CopyTo(0, null, 0, 0) @>)
        Assert.IsNotNull(m)
        Assert.AreEqual("CopyTo", m.Name)