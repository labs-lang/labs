// Learn more about F# at http://fsharp.org
open Base
open Checks
open Encode
open ArgParse
open Argu

[<EntryPoint>]
let main argv =

    let parsedCli = argv |> parseCLI

    let filename = 
        parsedCli 
        |> Result.map (fun (args, _) -> (args.GetResult <@ File @>)) 

    let bound = 
        parsedCli 
        |> Result.map (fun (args, _) -> (args.GetResult <@ Bound @>))

    parsedCli
    |> Result.map (setPlaceholders << snd)
    |> ignore

    filename
    >>= readFile
    >+> (parsedCli |> Result.map snd)
    >>= parse
    |> log "Parse successful"
    >>= checkNames
    |> log "All names are defined"

    //|> log "Init valid"
    >>= checkComponents
    |> log "All components are valid"
    >>= (encode <&> analyzeKeys)
    >+> bound
    >>= translateHeader
    >>= translateAll 
    >>= translateMain
    |> logErr // Log any error at the end
    |> setReturnCode
