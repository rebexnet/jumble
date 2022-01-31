namespace Jumble.Tests
open NUnit.Framework

open Jumble
open Jumble.Analysis

module TypeTreeTests =
    [<Test>]
    let ``Inner classes are included in type tree`` () = 
        let lh = R.loadHelper()    
        let tt = TypeTree(lh.AssemblyCache)
        let ttn = TypeSearch.findTypeNode<LibA.Outer.Inner<_>> tt.AllTypes 
        Assert.IsNotNull(ttn)