namespace AdventOfCode2023.Common

[<RequireQualifiedAccess>]
module Int =
    let isBetweenIncl min max value = min <= value && value <= max
    let diffMax a b max = (abs (a - b)) <= max

[<RequireQualifiedAccess>]
module Int64 =
    let diffMax<'t when 't: comparison> d (max: 't) (a: 't) (b: 't) =
        let diff = if a > b then d a b else d b a
        diff <= max
