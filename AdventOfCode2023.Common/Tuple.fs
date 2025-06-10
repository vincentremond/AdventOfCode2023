namespace AdventOfCode2023.Common

[<RequireQualifiedAccess>]
module Tuple =
    let mk2 f1 f2 x = (f1 x, f2 x)

[<RequireQualifiedAccess>]
module Tuple2 =
    let apply f (x, y) = f x y

    let bind f (x, y) = f x y
