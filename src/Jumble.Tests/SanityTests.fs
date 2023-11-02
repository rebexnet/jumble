module Jumble.Tests.SanityTests


open Jumble
open NUnit.Framework
open FsUnit

type SanityTests() as this =
    inherit CecilTestsBase()

    let ftd = this.LH.FindTypeDef

    [<Test>]
    member _.``When a class implements an interface I2 which is a descendant of I1 then the class also directly implements I1``() =
        let type_C = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.C>
        let type_I1 = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.I1>
        let type_I2 = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.I2>

        let interfaceDefinitions = type_C.Interfaces |> Seq.map (fun i -> i.InterfaceType.Resolve()) |> Seq.toArray

        interfaceDefinitions |> should equivalent [type_I1; type_I2]

    [<Test>]
    member _.``When an interface implements an interface I2 which is a descendant of I1 then the interface also directly implements I1``() =
        let type_I3 = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.I3>
        let type_I1 = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.I1>
        let type_I2 = ftd typedefof<LibA.Sanity.ChainedInterfaceImplementation.I2>

        let interfaceDefinitions = type_I3.Interfaces |> Seq.map (fun i -> i.InterfaceType.Resolve()) |> Seq.toArray

        interfaceDefinitions |> should equivalent [type_I1; type_I2]

    [<Test>]
    member _.``Resolving method reference on a type when the method is on its parent will resolve method on parent``() =
        let type_C1 = ftd typedefof<LibA.Sanity.ResolvingMethodOnChildType.C1>
        let methodDefinition = type_C1.Methods |> Seq.filter (fun m -> m.Name = "Method") |> Seq.exactlyOne
        let type_C2 = ftd typedefof<LibA.Sanity.ResolvingMethodOnChildType.C2>
        let methodReference = Mono.Cecil.MethodReference("Method", methodDefinition.ReturnType, type_C2)
        let resolved = methodReference.Resolve()

        resolved |> should equal methodDefinition

    // Silly test, but JetBrains Rider actually fails this
    [<Test>]
    member _.``Interface method override of interface default method implementation does actually override``() =
        let c12 = LibA.Sanity.InterfaceDefaultImpls.C12()
        c12.TestAsI1() |> should equal "I2"
        c12.TestAsI2() |> should equal "I2"

        let c2 = LibA.Sanity.InterfaceDefaultImpls.C2()
        c2.TestAsI1() |> should equal "I2"
        c2.TestAsI2() |> should equal "I2"

        let c123 = LibA.Sanity.InterfaceDefaultImpls.C123()
        c123.TestAsI1() |> should equal "I3"
        c123.TestAsI2() |> should equal "I3"
        c123.TestAsI3() |> should equal "I3"

        // TIL: Class cannot inherit interfaces with more than one default method implementation (cannot compile)

    [<Test>]
    member _.``When C2 :> C1 :> I then implicit method impl on C2 does not override I.Method()``() =
        let c2 = LibA.Sanity.ExplicitMethodImplWithChildImplicitImpl.C2()
        c2.Method() |> should equal "C2"
        (c2 :> LibA.Sanity.ExplicitMethodImplWithChildImplicitImpl.I).Method() |> should equal "C1"

    [<Test>]
    member _.``When C2 :> C1 :> I and also C2 :> I then implicit method impl on C2 does override I.Method()``() =
        let c2i = LibA.Sanity.ExplicitMethodImplWithChildImplicitImpl.C2I()
        c2i.Method() |> should equal "C2I"
        (c2i :> LibA.Sanity.ExplicitMethodImplWithChildImplicitImpl.I).Method() |> should equal "C2I"