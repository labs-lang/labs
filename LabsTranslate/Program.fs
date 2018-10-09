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
        async { 
            let! all = async { 
                let! arr = translateAll x
                return arr 
                    |> Seq.reduce (
                        fun r1 r2 -> 
                            r1 >+> r2
                            |> Result.map (fun (s1, s2) -> s1 + s2))
            }
            let! head = async { return (translateHeader isSim (x, b)) } |> Async.StartChild
            let! init = async { return translateInit x } |> Async.StartChild
            let! canProced = async { return translateCanProceed x } |> Async.StartChild
            let! main = async { return translateMain isFair x } |> Async.StartChild

            let! h = head
            let! i = init
            let! c = canProced
            let! m = main

            return 
                [|h; i; c; m|]
                |> Array.fold
                    (fun state x ->
                        state <&&> Result.map (printfn "%s") x)
                    (Result.Ok ())
        }
        |> Async.RunSynchronously
        
    let doTranslate cli =

        let isFair = 
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Fair @>)
            |> function Ok true -> true | _ -> false

        let isSimulation =
            parsedCli
            |> Result.map (fun args -> args.Contains <@ Simulation @>)
            |> function Ok true -> true | _ -> false
            
        (filenameOf cli)
        >>= readFile
        >+> (placeholders cli)
        >>= parse
        >>= checks
        >>= resolveSystem
        >>= encode
        >+> (bound cli)
        >>= (translate isSimulation isFair)
        |> logErr // Log any error at the end
        |> setReturnCode

    let doInfo cli = 
        (filenameOf cli)
        >>= readFile
        >+> (placeholders cli)
        >>= parse
        >>= checks
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