open System.IO
open FParsec

open Frontend
open Frontend.Outcome
open Frontend.Message
open LabsTranslate.Encode
open LabsTranslate.ArgParse
open Argu

let wrapParserResult p text =
    try
        let x = run p text
        match x with
        | Success(a, _, _) -> zero a
        | Failure(errorMsg, _, _) ->
            Outcome.Error([], [{What=Parser errorMsg; Where=[]}])
    with
        ex -> Outcome.Error([], [{What=Generic ex.Message; Where=[]}])


[<EntryPoint>]
let main argv =
    let flags (cli:ParseResults<_>) = (
        cli.Contains Fair,
        cli.Contains No_Bitvector,
        cli.Contains Simulation,
        cli.Contains Sync,
        cli.Contains No_Properties)
    try
        zero argv
        <~> (parseCLI >> zero)
        <~> fun cli ->
            let prop = cli.TryGetResult Property
            let input = File.ReadAllText (cli.GetResult File)
            let externs = getExterns cli |> Map.mapValues int
            (wrapParserResult Parser.parse input <~> Frontend.run externs) <~> fun x -> zero (cli, x)
        <?> (fun (cli, x) ->
            if cli.Contains Info then zero (x.Dump(prop))
            else
                let bound = cli.GetResult (Bound, defaultValue=1)
                let enc = cli.GetResult (Enc, defaultValue=C)
                encode enc bound (flags cli) prop x)
        |> function
           | Result.Ok (_, warns) ->
                warns |> List.map(pprintWarn >> eprintfn "%s") |> ignore
                0
           | Result.Error (warns, errs) ->
               warns |> List.map(pprintWarn >> eprintfn "%s") |> ignore
               errs |> List.map(pprintErr >> eprintfn "%s") |> ignore
               1 // TODO more expressive error codes 
    with
    | ex ->
        eprintfn $"{ex.Message}"
        1