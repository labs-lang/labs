module LabsTranslate.Program

open System.IO
open System.Text.Json
open FParsec

open Frontend
open Frontend.Outcome
open Frontend.Message
open LabsCore.ExprTypes
open LabsCore.Grammar
open LabsTranslate.Json
open LabsTranslate.TranslationKit
open LabsTranslate.Encode
open LabsTranslate.ArgParse

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
    try
        zero argv
        <~> (parseCLI >> zero)
        <~> fun cli ->
            let prop = cli.TryGetResult Property
            let input = File.ReadAllText (cli.GetResult File)
            let externs = getExterns cli |> Map.mapValues int
            (wrapParserResult Parser.parse input <~> Frontend.run externs) <~> fun x -> zero (cli, x)
        <?> (fun (cli, table) ->
            if cli.Contains Info then zero (table.Dump(prop) |> printf "%s")
            elif cli.Contains Only_Ast then
                zero( printfn $"%s{JsonSerializer.Serialize({table with Info = Some <| table.Dump(prop)}, JsonOptions table)}" )
            else
                let bound = cli.GetResult (Bound, defaultValue=1)
                let enc = cli.GetResult (Enc, defaultValue=C)
                match cli.TryGetResult Ast with
                | Some path ->
                    let json = JsonSerializer.Serialize({table with Info = Some <| table.Dump(prop)}, JsonOptions table)
                    use writer = new StreamWriter(path)
                    writer.Write(json)
                | None -> ()
                encode enc bound cli prop table
            )
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