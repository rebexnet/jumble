module Jumble.Tests.ManualPerfTests

open Jumble
open Jumble.Console
open NUnit.Framework

let private inputFile = @""

[<Test>]
let ``No input file committed`` () =
    Assert.That(inputFile, Is.EqualTo "", "You forgot to delete inputFile")

[<Test>]
[<Explicit>]
let ``Manual perf test`` () =
    let baseDir = System.IO.Path.GetDirectoryName(inputFile)
    let obfParams = ConfigurationModel.load inputFile |> ConfigurationModel.toObfuscationOptions baseDir
    Integration.obfuscate (fun _ -> obfParams) |> ignore