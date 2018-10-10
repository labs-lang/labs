open Base
open Checks
open EncodeInit
open Encode
open Templates
open ArgParse

[<EntryPoint>]
let main argv =

    let parsedCli = argv |> parseCLI

    let checks sys = 
        async {
            let! t1 = async { return checkNames sys } |> Async.StartChild
            let! t2 = async { return checkComponents sys } |> Async.StartChild
            let! t3 = async { return analyzeKeys sys } |> Async.StartChild
            let! r1 = t1
            let! r2 = t2
            let! r3 = t3

            return (r1 <&&> r2 <&&> r3 >>= fun m -> Ok (sys, m))
        }
        |> Async.RunSynchronously

    let translate isSim isFair (x, b) = 
        let _, trees, mapping = x
        [
            translateHeader isSim (x, b)
            translateInit x
            translateAll (trees, mapping)
            translateCanProceed (trees, mapping)
            translateMain isFair x
        ]
        |> List.map logErr
        |> List.map (Result.map (printfn "%s"))
        |> List.reduce (<&&>)

        
    let readAndCheck =
        (filenameOf parsedCli)
        >>= readFile
        >+> (placeholders parsedCli)
        >>= parse
        |> logErr
        >>= checks

    let doTranslate x =
        let bound =
            parsedCli
            |> Result.map (fun args -> args.GetResult <@ Bound @>) 

        let isFair = 
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Fair @>)
            |> function Ok true -> true | _ -> false

        let isSimulation =
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Simulation @>)
            |> function Ok true -> true | _ -> false

        x
        >>= resolveSystem
        >>= encode
        >+> bound
        >>= (translate isSimulation isFair)
        |> logErr // Log any error at the end
        |> setReturnCode

    let doInfo x = 
        x
        >>= serializeInfo
        |> logErr // Log any error at the end
        |> setReturnCode

    readAndCheck
    >+> parsedCli
    |> Result.map (fun (x, parsed) ->
        if parsed.Contains <@ Info @>
        then doInfo (Ok x)
        else doTranslate (Ok x)
    )
    |> function Ok i -> i | Error _ -> 10