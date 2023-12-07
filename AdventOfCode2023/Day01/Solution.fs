module AdventOfCode2023.Solution.Day01

open FSharp.Data.LiteralProviders
open FParsec
open AdventOfCode2023.Common.FParsec
open AdventOfCode2023.Common
open NUnit.Framework

type Char =
    | MiscLetter of char
    | Digit of int
    | SpelledDigit of int

let parseTextDigit (text: string) (value: int) =
    attempt ((pchar (text[0])) .>> (followedBy (pstring (text[1..]))) >>% SpelledDigit value)

let parseLine =
    many1 (
        (digiti |>> Digit)
        <|> (parseTextDigit "zero" 0)
        <|> (parseTextDigit "one" 1)
        <|> (parseTextDigit "two" 2)
        <|> (parseTextDigit "three" 3)
        <|> (parseTextDigit "four" 4)
        <|> (parseTextDigit "five" 5)
        <|> (parseTextDigit "six" 6)
        <|> (parseTextDigit "seven" 7)
        <|> (parseTextDigit "eight" 8)
        <|> (parseTextDigit "nine" 9)
        <|> (letter |>> MiscLetter)
    )

let parser = runParser (sepBy1 parseLine newline)

let solution digitSelector text =
    let parsed = parser text
    printfn "%A" parsed
    let lines = parsed |> List.map (List.choose digitSelector)

    let int32s =
        lines
        |> List.map ((Tuple.mk2 (List.head >> ((*) 10)) List.last) >> (Tuple2.apply (+)))

    int32s |> List.sum

[<Test>]
let ``part 1`` () =
    let chooser =
        function
        | MiscLetter _ -> None
        | Digit v -> Some v
        | SpelledDigit _ -> None
    
    test 142 (solution chooser TextFile.Day01.data.sample1.Text)
    test 55090 (solution chooser TextFile.Day01.data.personnal.Text)

[<Test>]
let ``part 2`` () =
    let chooser =
        function
        | MiscLetter _ -> None
        | Digit v -> Some v
        | SpelledDigit v -> Some v

    test 281 (solution chooser TextFile.Day01.data.sample2.Text)
    test 54845 (solution chooser TextFile.Day01.data.personnal.Text)
