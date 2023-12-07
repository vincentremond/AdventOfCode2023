namespace AdventOfCode2023.Common

open NUnit.Framework

[<AutoOpen>]
module NUnit =
    let test (expected: 'a) (actual: 'a): unit = Assert.That(actual, Is.EqualTo(expected))

