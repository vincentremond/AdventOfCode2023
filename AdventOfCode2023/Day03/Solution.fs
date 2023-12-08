module AdventOfCode2023.Solution.Day03

open FSharp.Data.LiteralProviders
open FParsec
open AdventOfCode2023.Common.FParsec
open AdventOfCode2023.Common
open NUnit.Framework

[<Measure>]
type line

module Line =
    let ofInt64 = (*) 1L<line>

[<Measure>]
type column

module Column =
    let ofInt64 = (*) 1L<column>

type PuzzleToken =
    | Number of int * int64<line> * int64<column> * int64<column>
    | Dot
    | Symbol of char * int64<line> * int64<column>

    static member number value line columnStart columnEnd =
        Number(value, Line.ofInt64 line, (Column.ofInt64 columnStart), (Column.ofInt64 columnEnd))

    static member symbol value line column =
        Symbol(value, Line.ofInt64 line, Column.ofInt64 column)

    member this.tryNumber =
        match this with
        | Number(value, line, columnStart, columnEnd) -> Some(value, line, columnStart, columnEnd)
        | _ -> None

    member this.trySymbol =
        match this with
        | Symbol(value, line, column) -> Some(value, line, column)
        | _ -> None

module Parser =

    // let withPosition p =
    //     pipe2 getPosition p (fun pos (length, token) -> PositionedToken.mk pos.Line pos.Column (pos.Column + length - 1L) token)

    let parseLine =
        many (
            choice [
                (pchar '.') >>% Dot
                pipe2
                    getPosition
                    (many1Chars digit)
                    (fun pos s -> PuzzleToken.number (int s) pos.Line pos.Column (pos.Column + (int64 s.Length) - 1L))
                pipe2 getPosition pchar' (fun pos c -> PuzzleToken.symbol c pos.Line pos.Column)
            ]
        )

    let exec = runParser (sepBy1 parseLine newline)

let solutionPart1 (input: string) =
    let parsed = input |> Parser.exec
    let all = parsed |> List.concat

    let symbolsPosition =
        all
        |> List.choose (_.trySymbol)
        |> List.map (fun (_, line, column) -> (line, column))

    let numbers = all |> List.choose (_.tryNumber)
    // get the numbers that have a symbol next to them
    let numbersWithSymbol =
        numbers
        |> List.choose (fun (value, line, columnStart, columnEnd) ->
            let lineRange = (line - 1L<line>), (line + 1L<line>)
            let columnRange = (columnStart - 1L<column>), (columnEnd + 1L<column>)

            let lineRangeCheck = ((Tuple2.bind Int.isBetweenIncl) lineRange)
            let columnRangeCheck = ((Tuple2.bind Int.isBetweenIncl) columnRange)

            let symbolExistsNextToNumber =
                symbolsPosition
                |> List.exists (fun (symbolLine, symbolColumn) ->
                    symbolLine |> lineRangeCheck && symbolColumn |> columnRangeCheck
                )

            if symbolExistsNextToNumber then Some value else None
        )

    numbersWithSymbol |> List.sum

let solutionPart2 (input: string) =
    let parsed = input |> Parser.exec
    let all = parsed |> List.concat

    let gears =
        all
        |> List.choose (_.trySymbol)
        |> List.choose (fun (symbol, line, column) ->
            match symbol with
            | '*' -> Some(line, column)
            | _ -> None
        )

    let numbers = all |> List.choose (_.tryNumber)

    gears
    |> List.choose (fun (gearLine, gearColumn) ->
        let numbersInRange =
            numbers
            |> List.choose (fun (value, numberLine, numberColumnStart, numberColumnEnd) ->
                let diffMax = (Int64.diffMax (-) 1L<line>)
                let lineIsInRange = (numberLine, gearLine) ||> diffMax

                let columnIsInRange =
                    (gearColumn >= (numberColumnStart - 1L<column>))
                    && (gearColumn <= (numberColumnEnd + 1L<column>))

                match lineIsInRange, columnIsInRange with
                | true, true -> Some value
                | _ -> None
            )

        match numbersInRange with
        | [ a; b ] -> Some(a * b)
        | _ -> None
    )
    |> List.sum

[<Test>]
let ``part 1 test`` () =
    test 4361 (solutionPart1 TextFile.Day03.test.Text)

[<Test>]
let ``part 1 user`` () =
    test 538046 (solutionPart1 TextFile.Day03.user.Text)

[<Test>]
let ``part 2 test`` () =
    test 467835 (solutionPart2 TextFile.Day03.test.Text)

[<Test>]
let ``part 2 user`` () =
    test 81709807 (solutionPart2 TextFile.Day03.user.Text)
    