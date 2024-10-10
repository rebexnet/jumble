module Jumble.Tests.Util.SeqTests
open Jumble
open NUnit.Framework

[<Test>]
let ``trySingle returns None for empty sequence`` () =
    Seq.empty |> Seq.trySingle |> assert_equal None

[<Test>]
let ``trySingle returns Some for single element sequence`` () =
    Seq.singleton 0 |> Seq.trySingle |> assert_equal (Some 0)

[<Test>]
let ``trySingle throws for multi element sequence`` () =
    let f() = [1;2] |> Seq.trySingle |> ignore
    Assert.That(f, Throws.InvalidOperationException)