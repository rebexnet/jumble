namespace Jumble.Tests.Cecil

open NUnit.Framework
open Jumble
open Jumble.Tests

[<TestFixture>]
type OperatorTests () as this = 
    inherit CecilTestsBase()

    let ftd = this.LH.FindTypeDef
    let type_CA1 = ftd typedefof<LibA.CA1_InheritsIA>
    let type_CB1 = ftd typedefof<LibB.CB1_InheritsIA>
    
    [<Test>]
    member _.``TypeReferences from different types are equal when referencing same type`` () = 
        let refFromCA1 = type_CA1.Interfaces[0].InterfaceType
        let refFromCB1 = type_CB1.Interfaces[0].InterfaceType

        Assert.IsNotNull(refFromCA1)
        Assert.IsNotNull(refFromCB1)
        Assert.IsTrue(TypeReference.areEqual (refFromCA1.Resolve()) (refFromCB1.Resolve()), "Resolved references are not equal")
        Assert.IsTrue(TypeReference.areEqual refFromCA1 refFromCB1, "References are not equal")