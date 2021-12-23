module NameGeneratorTests

open NUnit.Framework
open Jumble.Rename

[<Test>]
let ``Upside down`` () = 
    let text = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let expected = "Z⅄XMΛ∩┴SɹQԀONW˥ʞſIHפℲƎpƆq∀zʎxʍʌnʇsɹbdouɯlʞɾᴉɥƃɟǝpɔqɐ"
    Assert.AreEqual(expected, NameGenerators.upsideDown text)