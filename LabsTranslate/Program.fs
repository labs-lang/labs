open System.IO
open FParsec

open Frontend
open Frontend.Outcome
open Frontend.Message
open LabsToC
open ArgParse
open Argu
open Types

let wrapParserResult p text =
    try
        let x = CharParsers.run p text
        match x with
        | Success(a, _, _) -> zero a
        | Failure(errorMsg, _, _) ->
            Outcome.Error([], [{what=Parser errorMsg; where=[]}])
    with
        ex -> Outcome.Error([], [{what=Generic ex.Message; where=[]}])


[<EntryPoint>]
let main argv =
    let flags (cli:ParseResults<_>) = (cli.Contains Fair, cli.Contains No_Bitvector, cli.Contains Simulation, cli.Contains Sync)
    
    zero argv
    <~> (parseCLI >> zero)
    <~> fun cli ->
        let input = File.ReadAllText (cli.GetResult Arguments.File)
        let externs = getExterns cli |> Map.mapValues int
        (wrapParserResult Parser.parse input <~> Frontend.run externs) <~> fun x -> zero (cli, x)
    <?> (fun (cli, x) ->
        if cli.Contains Info then zero (x.dump())
        else
            let bound = cli.GetResult (Bound, defaultValue=1)
            let enc = cli.GetResult (Enc, defaultValue=C)
            LabsToC.encode enc (bound) (flags cli) x)
    |> Result.mapError (eprintfn "%A") // TODO Format errors and set exit code
    |> ignore
    0 // return an integer exit code
