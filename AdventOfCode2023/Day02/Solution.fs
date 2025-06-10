module AdventOfCode2023.Solution.Day02

open FSharp.Data.LiteralProviders
open FParsec
open AdventOfCode2023.Common.FParsec
open AdventOfCode2023.Common
open NUnit.Framework

type Color =
    | Green
    | Blue
    | Red

type Description = {
    Red: int
    Green: int
    Blue: int
} with

    static member Zero = {
        Red = 0
        Green = 0
        Blue = 0
    }

type Game = {
    GameId: int
    Draws: Description list
}

let parseDescription =
    sepBy
        ((pint32 .>> skipString " ")
         .>>. ((pstring "red" >>% Red)
               <|> (pstring "green" >>% Green)
               <|> (pstring "blue" >>% Blue)))
        (skipString ", ")
    |>> List.fold
            (fun acc (count, color) ->
                match color with
                | Green -> { acc with Green = acc.Green + count }
                | Blue -> { acc with Blue = acc.Blue + count }
                | Red -> { acc with Red = acc.Red + count }
            )
            Description.Zero

let parseGame =
    pipe2
        (skipString "Game " >>. pint32 .>> skipString ": ")
        (sepBy1 parseDescription (skipString "; "))
        (fun gameId draws -> {
            GameId = gameId
            Draws = draws
        })

let parser = runParser (sepBy1 parseGame newline)

let solutionPart1 text =
    let games = text |> parser

    let initialCount = {
        Red = 12
        Green = 13
        Blue = 14
    }

    let gameIsValid game =
        let isDrawValid draw =
            draw.Red <= initialCount.Red
            && draw.Green <= initialCount.Green
            && draw.Blue <= initialCount.Blue

        game.Draws |> List.forall isDrawValid

    games |> List.where gameIsValid |> List.map (_.GameId) |> List.sum

let solutionPart2 text =
    let games = text |> parser

    let mergeDescription desc1 desc2 = {
        Red = max desc1.Red desc2.Red
        Green = max desc1.Green desc2.Green
        Blue = max desc1.Blue desc2.Blue
    }

    games
    |> List.map (fun game -> game.Draws |> List.reduce mergeDescription)
    |> List.map (fun desc -> desc.Red * desc.Green * desc.Blue)
    |> List.sum

[<Test>]
let ``part 1`` () =
    test 8 (solutionPart1 TextFile.Day02.test.Text)
    test 2593 (solutionPart1 TextFile.Day02.user.Text)

[<Test>]
let ``part 2`` () =
    test 2286 (solutionPart2 TextFile.Day02.test.Text)
    test 54699 (solutionPart2 TextFile.Day02.user.Text)
