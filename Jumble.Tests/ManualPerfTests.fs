module Jumble.Tests.ManualPerfTests

open Jumble
open NUnit.Framework

let private inputFile = @""

[<Test>]
let ``No input file committed`` () =
    Assert.AreEqual("", inputFile, "You forgot to delete inputFile")

[<Test>]
[<Explicit>]
let ``Manual perf test`` () =
    let baseDir = System.IO.Path.GetDirectoryName(inputFile)
    let obfParams = ConfigurationModel.load inputFile |> ConfigurationModel.toObfuscationOptions baseDir
    obfuscate (fun _ -> obfParams) |> ignore