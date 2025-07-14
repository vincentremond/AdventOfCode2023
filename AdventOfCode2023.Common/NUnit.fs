namespace AdventOfCode2023.Common

open NUnit.Framework
open FsUnitTyped

[<AutoOpen>]
module NUnit =
    let test (expected: 'a) (actual: 'a) : unit =
        actual |> shouldEqual expected
        Assert.Pass($"%A{actual}")
