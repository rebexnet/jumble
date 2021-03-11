module InterfaceImplLookupTests

open Mono.Cecil
open NUnit.Framework
open System
open System.Globalization

open Jumble.Analysis
open Jumble
open Jumble.Tests

type InterfaceImplLookupTests () as this = 
    inherit CecilTestsBase()

    let ftd = this.LH.FindTypeDef
    let type_CultureInfo = ftd typedefof<CultureInfo>
    let type_DateTime = ftd typedefof<DateTime>
    let type_CComplexGenericDouble = ftd typeof<LibA.Complex.CComplexGenericDouble>
    let type_CComplexSimple2 = ftd typeof<LibA.Complex.CComplexSimple2>
    let type_CComplexSimpleExplImpl = ftd typeof<LibA.Complex.CComplexSimpleExplImpl>
    let method_IComplexGeneric_MethodSimple = this.LH.FindMethodDefs typedefof<LibA.Complex.IComplexGeneric<_,_>> "MethodSimple" |> List.exactlyOne

    [<Test>]
    member __.``tryFindExplicitMethodImplementation finds method in same type`` () = 
        let parentRef = type_CComplexSimpleExplImpl.Interfaces.[0].InterfaceType
        let method = method_IComplexGeneric_MethodSimple
        let expectedMethod = type_CComplexSimpleExplImpl.Methods |> Seq.find (fun m -> m.Name.Contains("Simple"))
        Assert.IsNotNull(expectedMethod)

        match MethodLookup.tryFindExplicitMethodImplementation type_CComplexSimpleExplImpl parentRef method with 
        | None -> Assert.Fail()
        | Some m -> Assert.AreEqual(expectedMethod, m)
    
    [<Test>]
    member __.``tryFindExplicitMethodImplementation finds method in another type`` () = 
        let parentRef = type_CComplexSimple2.Interfaces.[0].InterfaceType
        let method = method_IComplexGeneric_MethodSimple
        let expectedMethod = type_CComplexGenericDouble.Methods |> Seq.find (fun m -> m.Name.Contains("Simple") && TypeReference.areEqual m.Parameters.[0].ParameterType type_DateTime)
        Assert.IsNotNull(expectedMethod)

        match MethodLookup.tryFindExplicitMethodImplementation type_CComplexGenericDouble parentRef method with 
        | None -> Assert.Fail()
        | Some m -> Assert.AreEqual(expectedMethod, m)
        
    [<Test>]
    member __.``findMethodImplementation finds method implicit implementation method, same type`` () = 
        let parentRef = type_CComplexSimple2.Interfaces |> Seq.exactlyOne |> (fun i -> i.InterfaceType)
        let method = method_IComplexGeneric_MethodSimple
        let expectedMethod = type_CComplexSimple2.Methods |> Seq.find(fun m -> m.Name.Contains("Simple"))
        Assert.IsNotNull(expectedMethod)

        let foundMethod = MethodLookup.findInterfaceMethodImplementationViaReference type_CComplexSimple2 parentRef method
        Assert.AreEqual(expectedMethod, foundMethod)

    [<Test>]
    member __.``findMethodImplementation finds method explicit implementation method (and prefers over implicit), same type`` () = 
        let parentRef = type_CComplexGenericDouble.Interfaces 
                        |> Seq.map(fun i -> i.InterfaceType) 
                        |> Seq.choose(function | :? GenericInstanceType as i -> Some i | _ -> None)
                        |> Seq.find(fun i -> i.GenericArguments.Count > 0 && TypeReference.areEqual i.GenericArguments.[0] type_DateTime)
        let method = method_IComplexGeneric_MethodSimple
        //  CultureInfo IComplexGeneric<DateTime, CultureInfo>.MethodSimple(DateTime tval)
        let expectedMethod = type_CComplexGenericDouble.Methods |> Seq.find(fun m -> m.Name.Contains("Simple") && TypeReference.areEqual m.ReturnType type_CultureInfo && m.Name <> "MethodSimple")
        Assert.IsNotNull(expectedMethod)

        let foundMethod = MethodLookup.findInterfaceMethodImplementationViaReference type_CComplexGenericDouble parentRef method
        Assert.AreEqual(expectedMethod, foundMethod)