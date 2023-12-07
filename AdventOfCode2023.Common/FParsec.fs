module AdventOfCode2023.Common.FParsec

open FParsec
open FParsec.CharParsers

let crlf: Parser<unit, unit> = newline >>. skipChar '\r'

let digiti: Parser<int, unit> =
    digit
    |>> (fun char -> (int char) - (int '0'))

let runParser parser input =
    match run (parser .>> eof) input with
    | Success(result, _, _) -> result
    | Failure(msg, _, _) -> failwith msg
