module ParameterParserTests

open Jumble
open Jumble.Parameter
open Jumble.Analysis.Parsers.PParameter
open Jumble.Analysis.Parsers.PShared
open NUnit.Framework

let private toSimple name = name |> SimpleType |> SimpleParameter
let private compare pString expected = 
    Assert.AreEqual(expected, parseParameter pString |> toRes)

let private compareN pString expected = 
    Assert.AreEqual(expected, parseNamedParameter pString |> toRes)


[<Test>]
let ``Array type`` () = 
    let expected  = (Parameter.toSimple "A.B") |> Parameter.toArray 3 |> Parameter.toArray 1
    compare "A.B[,,][]" expected

[<Test>]
let ``ByRef type - ref`` () = 
    let expected = Parameter.toRef (Parameter.toSimple "A.B")
    compare "ref A.B" expected

[<Test>]
let ``ByRef type - out`` () = 
    let expected = Parameter.toOut (Parameter.toSimple "A.B")
    compare "out A.B" expected

[<Test>]
let ``Generic type`` () = 
    let expected = 
        let d,e,g = Parameter.toSimple "D", Parameter.toSimple "E", Parameter.toSimple "G"
        let c,f = Parameter.toGeneric "C" [d;e], Parameter.toGeneric "F" [g]
        Parameter.toGeneric "A.B" [c;f]
        
    compare "A.B<C<D,E>,F<G>>" expected

[<Test>]
let ``Pointer type`` () = 
    let expected = Parameter.toSimple "A.B" |> Parameter.toPointer |> Parameter.toPointer 
    compare "A.B**" expected

[<Test>]
let ``Simple type`` () = 
    compare "Foo.Bar" (toSimple "Foo.Bar")

[<Test>]
let ``Named simple type`` () = 
    compareN "Foo.Bar name" (toSimple "Foo.Bar" |> NamedParameter.create "name")

[<Test>]
let ``The beast`` () = 
    let e = toSimple "E" |> toPointer |> toPointer |> toArray 1
    let d = toSimple "D" |> toArray 2 |> toPointer
    let c = toGeneric "C" [d] |> toPointer
    let abGen = toGeneric "A.B" [c;e]
    let expected = abGen |> toPointer |> toArray 1 |> toArray 1 |> toArray 1 |> toPointer |> toRef
    compare "ref A.B<C<D[,]*>*, E**[]>*[][][]*" expected