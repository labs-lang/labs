open Base
open Checks
open Encode
open Templates
open ArgParse

[<EntryPoint>]
let main argv =

    let parsedCli = argv |> parseCLI

    let chosenTranslateMain = 
        let isFair = 
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Fair @>)
            |> function Ok(true) -> true | _ -> false
        if isFair then (translateMain fairInterleaving)
        else (translateMain fullInterleaving)

    (filename parsedCli)
    >>= readFile
    >+> (placeholders parsedCli)
    >>= parse
    //|> log "Parse successful"
    >>= checkNames
    //|> log "All names are defined"

    //|> log "Init valid"
    >>= checkComponents
    //|> log "All components are valid"
    >>= (encode <&> analyzeKeys)
    >+> (bound parsedCli)
    >>= translateHeader
    >>= translateAll 
    >>= chosenTranslateMain
    |> logErr // Log any error at the end
    |> setReturnCode
