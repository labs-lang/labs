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

            return (r1 <&&> r2 <&&> r3 >>= fun (m, (a,b,c)) -> Ok (sys, m, a,b,c))
        }
        |> Async.RunSynchronously

    let translate isSim isFair (x, b) = 
        let sys, trees, mapping, _, _, _ = x
        [
            translateHeader isSim (x, b)
            translateInit (sys, trees, mapping)
            translateAll trees
            translateCanProceed trees
            translateMain isFair (sys,trees)
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

    let doTranslate (s,m,maxI,maxL,maxE) =
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


        resolveSystem (s, m)
        >>= encode
        |> Result.map (fun (sys, trees) -> sys, trees, m, maxI,maxL,maxE)
        >+> bound
        >>= (translate isSimulation isFair)
        |> logErr // Log any error at the end
        |> setReturnCode

    let doInfo (s,m,_,_,_) = 
        serializeInfo (s,m)
        |> logErr // Log any error at the end
        |> setReturnCode

    readAndCheck
    >+> parsedCli
    |> Result.map (fun (x, parsed) ->
        if parsed.Contains <@ Info @>
        then doInfo x
        else doTranslate x
    )
    |> function Ok i -> i | Error _ -> 10