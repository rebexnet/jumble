module DeriveDownTests

open Mono.Cecil
open NUnit.Framework
open Jumble.Tests
open System.Collections.Generic

open Jumble
open Jumble.Analysis

type DeriveDownTests () as this =
    inherit CecilTestsBase()
    let ftd = this.LH.FindTypeDef
    let type_IComplexGeneric = ftd typedefof<LibA.Complex.IComplexGeneric<_,_>>
    let type_CA1 = ftd typedefof<LibA.CA1_InheritsIA>
    let type_float = ftd typedefof<float32>
    let type_int = ftd typedefof<int>
    let type_string = ftd typedefof<string>
    let type_ICollection_T = ftd typedefof<ICollection<_>>
    let type_IEnumerable_T = ftd typedefof<IEnumerable<_>>
    let type_IDictionary_T_U = ftd typedefof<IDictionary<_,_>>
    let type_CComplexGeneric_T = ftd typedefof<LibA.Complex.CComplexGeneric<_>>
    let type_CComplexGenericDouble = ftd typeof<LibA.Complex.CComplexGenericDouble>
    
    let method_IA_MethodA_string = this.LH.FindMethodDefs typedefof<LibA.IA> "MethodA" |> List.find (fun m -> m.Parameters.Count = 1)
    let method_IA_MethodGeneric = this.LH.FindMethodDefs typedefof<LibA.IA> "GenericMethod" |> List.exactlyOne
    let method_IA_MethodComplexParameter = this.LH.FindMethodDefs typedefof<LibA.IA> "MethodComplexParameter" |> List.exactlyOne
    let method_IComplexGeneric_MethodSimple = this.LH.FindMethodDefs typedefof<LibA.Complex.IComplexGeneric<_,_>> "MethodSimple" |> List.exactlyOne
    let method_IComplexGeneric_MethodComplex = this.LH.FindMethodDefs typedefof<LibA.Complex.IComplexGeneric<_,_>> "MethodComplex" |> List.exactlyOne

    let (=?=) (actual:TypeReference) (expected:TypeReference) = 
        Assert.IsNotNull(expected)
        Assert.IsNotNull(actual)

        Assert.IsTrue(TypeReference.areEqual actual expected)

    
    [<Test>]
    member _.``deriveParameter - simple specific (string MethodA() -> same)`` () =
        let parameter = method_IA_MethodA_string.Parameters[0].ParameterType
        let derivedParameter = DeriveDown.deriveType type_CA1.Interfaces[0].InterfaceType parameter

        let dp = TypeReference.safeResolve derivedParameter
        Assert.AreEqual(type_string, dp)

    [<Test>]
    member _.``deriveParameter - IEn<IColl<string>> MethodComplexParameter(IEn<IColl<string>> _) -> same`` () = 
        let parameter = method_IA_MethodComplexParameter.Parameters[0].ParameterType
        let derivedParameter = DeriveDown.deriveType type_CA1.Interfaces[0].InterfaceType parameter

        derivedParameter =?= type_IEnumerable_T
        
        let gp0 = (derivedParameter :?> GenericInstanceType).GenericArguments[0]
        gp0 =?= type_ICollection_T 
        
        (gp0 :?> GenericInstanceType).GenericArguments[0] =?= type_string

    [<Test>]
    member _.``deriveParamter - U MethodSimple(T tval) -> float MethodSimple(int tval)`` () = 
        let parameter = method_IComplexGeneric_MethodSimple.Parameters[0].ParameterType
        let deriveByRef = type_CComplexGenericDouble.Interfaces 
                          |> Seq.filter(fun i -> i.InterfaceType.Name = type_IComplexGeneric.Name 
                                                 && (i.InterfaceType :?> GenericInstanceType).GenericArguments[1].Name = type_float.Name)
                          |> Seq.exactlyOne
        let derivedParameter = DeriveDown.deriveType deriveByRef.InterfaceType parameter

        derivedParameter =?= type_int

    [<Test>]
    member _.``deriveParameter - U MethodSimple(T tval) -> IEnumerable<TT> MethodSimple(TT tval)`` () = 
        let parameter = method_IComplexGeneric_MethodSimple.Parameters[0].ParameterType
        let deriveByRef = type_CComplexGeneric_T.Interfaces
                          |> Seq.filter(fun i -> i.InterfaceType.Name = type_IComplexGeneric.Name)
                          |> Seq.exactlyOne

        let derivedParameter = DeriveDown.deriveType deriveByRef.InterfaceType parameter

        derivedParameter =?= type_CComplexGeneric_T.GenericParameters[0]

    [<Test>]
    member _.``deriveParameter - U MethodComplex(T tval, IDictionary<T, U> dict) -> float MethodComplex(int tval, IDictionary<int, float> dict)`` () = 
        let parameter = method_IComplexGeneric_MethodComplex.Parameters[1].ParameterType
        let deriveByRef = type_CComplexGenericDouble.Interfaces 
                          |> Seq.filter(fun i -> i.InterfaceType.Name = type_IComplexGeneric.Name 
                                                 && (i.InterfaceType :?> GenericInstanceType).GenericArguments[1].Name = type_float.Name)
                          |> Seq.exactlyOne
        
        // IDictionary<T,U> -> IDictionary<int, float>
        let derivedParameter = DeriveDown.deriveType deriveByRef.InterfaceType parameter

        derivedParameter =?= type_IDictionary_T_U
        let getGenericArg n = (derivedParameter :?> GenericInstanceType).GenericArguments[n]

        let gp0, gp1 = getGenericArg 0, getGenericArg 1
        gp0 =?= type_int
        gp1 =?= type_float

    [<Test>]
    member _.``applyGenericMapsToMethodDown - can resolve composite generic`` () = 
        let method = method_IComplexGeneric_MethodComplex
        let deriveByRef = type_CComplexGenericDouble.Interfaces
                          |> Seq.filter(fun i -> i.InterfaceType.Name = type_IComplexGeneric.Name
                                                 && (i.InterfaceType :?> GenericInstanceType).GenericArguments[0].Name = type_int.Name)
                          |> Seq.exactlyOne

        // IDictionary<T,U> -> IDictionary<int, float>
        let derivedMethod = DeriveDown.deriveMethodWithTarget deriveByRef.InterfaceType method type_CComplexGenericDouble

        let resolved = MethodReference.safeResolve derivedMethod
        let targetMethod = this.LH.FindMethodDefs typedefof<LibA.Complex.CComplexGenericDouble> "MethodComplex" 
                           |> Seq.filter (fun m -> m.Parameters[0].ParameterType.Name = type_int.Name) |> Seq.exactlyOne

        Assert.AreEqual(targetMethod, resolved)
       
    [<Test>]
    member _.``applyGenericMapsToMethodDown - can resolve self-generic method`` () = 
        let method = method_IA_MethodGeneric
        let deriveByRef = type_CA1.Interfaces |> Seq.exactlyOne
        // string GenericMethod<T>() -> string GenericMethod<T>()
        let derivedMethod = DeriveDown.deriveMethodWithTarget deriveByRef.InterfaceType method type_CA1

        Assert.IsNotNull(derivedMethod)
        let resolved = derivedMethod.Resolve()
        let expectedMethod = this.LH.FindMethodDefs typedefof<LibA.CA1_InheritsIA> "GenericMethod" |> Seq.exactlyOne
        Assert.IsNotNull(expectedMethod)
        Assert.IsNotNull(resolved)
        Assert.AreEqual(expectedMethod, resolved)