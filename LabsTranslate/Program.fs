﻿open Base
open Checks
open EncodeInit
open Encode
open Templates
open ArgParse

[<EntryPoint>]
let main argv =

    let parsedCli = argv |> parseCLI

    let doTranslate cli =
        let isFair = 
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Fair @>)
            |> function Ok(true) -> true | _ -> false

        let chosenInit =
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Simulation @>)
            |> function Ok(true) -> initVarSim | _ -> initVar
            |> translateInit

        (filenameOf cli)
        >>= readFile
        >+> (placeholders cli)
        >>= parse
        >>= checkNames
        >>= checkComponents
        >>= analyzeKeys
        >>= resolveSystem
        >>= encode
        >+> (bound cli)
        >>= translateHeader
        >>= chosenInit
        >>= translateAll 
        >>= (translateMain isFair)
        |> logErr // Log any error at the end
        |> setReturnCode

    let doInfo cli = 
        (filenameOf cli)
        >>= readFile
        >+> (placeholders cli)
        >>= parse
        >>= checkNames
        >>= checkComponents
        >>= analyzeKeys
        >>= serializeInfo
        |> logErr // Log any error at the end
        |> setReturnCode

    parsedCli
    |> logErr
    |> Result.map (fun args -> 
        if args.Contains <@ Info @>
        then doInfo parsedCli
        else doTranslate parsedCli)
    |> function Ok i -> i | Error _ -> 10