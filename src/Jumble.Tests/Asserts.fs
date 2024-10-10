[<AutoOpen>]
module Jumble.Asserts

open NUnit.Framework

let inline assert_notnull (x:^T) = Assert.That<^T>(x, Is.Not.Null)
let inline assert_none x = Assert.That(x, Is.EqualTo None)
let inline assert_equal expected (actual:^T) = Assert.That<^T>(actual, Is.EqualTo expected)
let inline assert_equivalent expected (actual:^T) = Assert.That<^T>(actual, Is.EquivalentTo expected)
let inline assert_not_equal expected (actual:^T) = Assert.That<^T>(actual, Is.Not.EqualTo expected)