open System.IO
open FParsec

open Checker.Outcome

let wrapParserResult p text =
    try
        let x = CharParsers.run p text
        match x with
        | Success(a, _, _) -> zero a
        | Failure(errorMsg, _, _) ->
            failwithf "Parsing failed:\n %s" errorMsg
    with
        ex -> failwith ex.Message

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText argv.[0]
    let externs = [("delta", 10); ("grid", 10); ("birds", 3)] |> Map.ofList
    
    wrapParserResult Parser.parse input
    <~> Checker.Transform.run externs
    |> eprintfn "%A"
    0 // return an integer exit code
