open Base
open Checks
open EncodeInit
open Encode
open Templates
open ArgParse


[<EntryPoint>]
let main argv =

    let checks sys = 
        async {
            let! t1 = async { return checkNames sys } |> Async.StartChild
            let! t2 = async { return checkComponents sys } |> Async.StartChild
            let! t3 = async { return analyzeKeys sys } |> Async.StartChild
            let! r1 = t1
            let! r2 = t2
            let! r3 = t3

            return (r1 <&&> r2 <&&> r3 >>= fun (m, (a, b, c)) -> Ok (sys, m, a,b,c))
        }
        |> Async.RunSynchronously


    let readAndCheck (cli:Argu.ParseResults<_>) =
        cli.GetResult File
        |> readFile
        >+> Ok (placeholders cli)
        >>= parse
        |> logErr
        >>= checks

    let doTranslate (cli:Argu.ParseResults<_>) (s, m, maxI, maxL, maxE) =
        let translate x = 
            let bound = cli.GetResult (Bound, defaultValue=1)
            let sys, trees, _, _, _, _ = x
            [
                translateHeader (cli.Contains Simulation) (cli.Contains No_Bitvector)  bound x
                translateInit (sys, trees, m)
                translateAll trees
                translateCanProceed trees
                translateMain (cli.Contains Fair) (sys, trees)
            ]
            |> List.map ((Result.map (printfn "%s")) >> logErr)
            |> List.reduce (<&&>)


        resolveSystem (s, m)
        >>= encode
        |> Result.map (fun (sys, trees) -> (sys, trees, m, maxI,maxL,maxE))
        >>= translate
        |> logErr // Log any error at the end
        |> setReturnCode

    let doInfo (s,m,_,_,_) = 
        serializeInfo (s, m)
        |> logErr // Log any error at the end
        |> setReturnCode

    try
        let parsedCli = parseCLI argv
        readAndCheck parsedCli
        |> Result.map (fun x ->
            if parsedCli.Contains Info
            then doInfo x
            else doTranslate parsedCli x
        )
        |> 
            function | Ok i -> i 
                     | Error _ -> 10
    with 
        e -> 
            eprintfn "%s" e.Message
            10