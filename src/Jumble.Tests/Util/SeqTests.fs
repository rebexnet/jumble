module Jumble.Tests.Util.SeqTests
open System
open Jumble
open FsUnit
open NUnit.Framework

[<Test>]
let ``trySingle returns None for empty sequence`` () =
    Seq.empty |> Seq.trySingle |> should equal None

[<Test>]
let ``trySingle returns Some for single element sequence`` () =
    Seq.singleton 0 |> Seq.trySingle |> should equal (Some 0)

[<Test>]
let ``trySingle throws for multi element sequence`` () =
    let f() = [1;2] |> Seq.trySingle |> ignore
    f |> should throw typeof<InvalidOperationException>