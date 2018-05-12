// Learn more about F# at http://fsharp.org
open Base
open Checks
open Encode

type args = {
    help: bool; 
    atomicLstig: bool; 
    filePath: string;
    verbose: bool
}
let defaults = {help = false; atomicLstig = false; filePath = ""; verbose = false}

let rec parseArgs partial args =
    match args with
    | [] -> Result.Ok(partial)
    | "--help" :: tail
    | "-h" :: tail -> Result.Ok({partial with help = true})
    | fpath :: tail when partial.filePath = "" -> 
        parseArgs {partial with filePath = fpath} tail
    | fpath :: tail -> Result.Error("Multiple files specified")

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    // return an integer exit code

    //let args = argv |> List.ofSeq |> parseArgs defaults
    let args = Result.Ok {
        help=false; 
        atomicLstig=false;
        filePath="../examples/test.labs";
        verbose=false
    }

    args 
    |> Result.map (fun x -> x.filePath)
    >>= readFile
    >>= parse
    |> (fun x -> printf "%A" x; x)
    |> log "Parse successful"
    .>>= uniqueDefs
    |> log "All definitions are unique"
    .>>= checkNames
    |> log "All names are defined"
    .>>= checkInit
    |> log "Init valid"
    >>= checkComponents
    |> Result.map (fun (procs, comps) -> (procs, comps, enumerateKeys (List.map snd procs)) )
    ///|> logErr // Log any error at the end
    |> ignore


    0