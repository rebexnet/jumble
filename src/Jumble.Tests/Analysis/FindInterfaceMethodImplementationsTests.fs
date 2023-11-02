module Jumble.Tests.Analysis.FindInterfaceMethodImplementationsTests

open FsUnit
open NUnit.Framework

open Jumble.Analysis
open Jumble
open Jumble.Tests

type InterfaceImplLookupTests () as this =
    inherit CecilTestsBase()

    let ftd = this.LH.FindTypeDef

    let test expected actual =
        actual |> should equivalent expected

    let f = InterfaceMethodImplSearch.findInterfaceMethodImplementations

    [<Test>]
    member _.``Implicit on same class``() =
        let i = ftd typeof<LibA.FindInterfaceMethodImpls.DirectImplicit.I>
        let c = ftd typeof<LibA.FindInterfaceMethodImpls.DirectImplicit.C>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Name = "Method" && m.Parameters.Count = 0) |> Seq.exactlyOne
        f im c |> should equal [expected]

    [<Test>]
    member _.``Implicit on parent class``() =
        let i = ftd typeof<LibA.FindInterfaceMethodImpls.ParentImplicit.I>
        let c1 = ftd typeof<LibA.FindInterfaceMethodImpls.ParentImplicit.C1>
        let c2 = ftd typeof<LibA.FindInterfaceMethodImpls.ParentImplicit.C2>
        let im = i.Methods |> Seq.exactlyOne
        let expected = TypeDefinition.findMethodSingle c1 (nameof(Unchecked.defaultof<LibA.FindInterfaceMethodImpls.ParentImplicit.C1>.Method))
        f im c2 |> should equal [expected]

    [<Test>]
    member _.``Explicit on same class``() =
        let i = ftd typeof<LibA.FindInterfaceMethodImpls.DirectExplicit.I>
        let c = ftd typeof<LibA.FindInterfaceMethodImpls.DirectExplicit.C>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Explicit on parent class``() =
        let i = ftd typeof<LibA.FindInterfaceMethodImpls.ParentExplicit.I>
        let c1 = ftd typeof<LibA.FindInterfaceMethodImpls.ParentExplicit.C1>
        let c2 = ftd typeof<LibA.FindInterfaceMethodImpls.ParentExplicit.C2>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c1.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c2 |> should equivalent [expected]

    [<Test>]
    member _.``Implicit generic with single argument on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.I<_>>
        let c = ftd typeof<LibA.FindInterfaceMethodImpls.GenericOneArg.CImpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = TypeDefinition.findMethodSingle c (nameof(Unchecked.defaultof<LibA.FindInterfaceMethodImpls.GenericOneArg.CImpl>.Method))
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Implicit generic with single argument on same generic class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.CGenImpl<_>>
        let im = i.Methods |> Seq.exactlyOne
        let expected = TypeDefinition.findMethodSingle c (nameof(Unchecked.defaultof<LibA.FindInterfaceMethodImpls.GenericOneArg.CGenImpl<_>>.Method))
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Explicit generic with single argument on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.CExpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Explicit generic with single argument on same generic class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericOneArg.CGenExpl<_>>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]
        
    [<Test>]
    member _.``Implicit generic with two arguments on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.I<_, _>>
        let c = ftd typeof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CImpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = TypeDefinition.findMethodSingle c (nameof(Unchecked.defaultof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CImpl>.Method))
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Implicit generic with two arguments on same generic class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.I<_, _>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CGenImpl<_, _>>
        let im = i.Methods |> Seq.exactlyOne
        let expected = TypeDefinition.findMethodSingle c (nameof(Unchecked.defaultof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CGenImpl<_,_>>.Method))
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Explicit generic with two arguments on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.I<_, _>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CExpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Explicit generic with two arguments on same generic class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.I<_, _>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.GenericMoreArgs.CGenExpl<_, _>>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Two explicit implementations of generic interface on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.CExplExpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.toList

        expected |> List.length |> should equal 2
        f im c |> should equivalent expected

    [<Test>]
    member _.``One explicit and one implicit implementations of generic interface on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.CImplExpl>
        let im = i.Methods |> Seq.exactlyOne
        let explMethod = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        let implMethod = c.Methods |> Seq.filter (fun m -> m.Name = "Method" && m.Parameters[0].ParameterType.Resolve() = ftd typedefof<int>) |> Seq.exactlyOne
        let expected = [explMethod; implMethod]
        f im c |> should equivalent expected

    [<Test>]
    member _.``Two implicit implementations of generic interface on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.I<_>>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.MultipleImplementation.CImplImpl>
        let im = i.Methods |> Seq.exactlyOne

        let expected = c.Methods
                       |> Seq.filter (fun m ->
                           m.Name = "Method" && m.Parameters[0].ParameterType.Resolve() <> ftd typedefof<char>)
                       |> Seq.toList

        expected |> Seq.length |> should equal 2
        f im c |> should equivalent expected

    [<Test>]
    member _.``Explicit method-level generic on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.MethodLevelGeneric.I>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.MethodLevelGeneric.CExpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.Overrides.Count > 0) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Implicit method-level generic on same class``() =
        let i = ftd typedefof<LibA.FindInterfaceMethodImpls.MethodLevelGeneric.I>
        let c = ftd typedefof<LibA.FindInterfaceMethodImpls.MethodLevelGeneric.CImpl>
        let im = i.Methods |> Seq.exactlyOne
        let expected = c.Methods |> Seq.filter (fun m -> m.HasGenericParameters) |> Seq.exactlyOne
        f im c |> should equivalent [expected]

    [<Test>]
    member _.``Default implementation on child interface``() =
        let i1 = ftd typedefof<LibA.FindInterfaceMethodImpls.InterfaceDefault.I1>
        let i2 = ftd typedefof<LibA.FindInterfaceMethodImpls.InterfaceDefault.I2>
        let im = i1.Methods |> Seq.exactlyOne
        let expected = i2.Methods |> Seq.exactlyOne
        f im i2 |> should equivalent [expected]