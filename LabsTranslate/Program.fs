// Learn more about F# at http://fsharp.org
open Base
open Checks
open Encode

type args = {
    help: bool; 
    atomicLstig: bool; 
    filePath: string;
    verbose: bool
    spawn: Map<string, int> 
}

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
    //let args = argv |> List.ofSeq |> parseArgs defaults
    let args = Result.Ok {
        help=false; 
        atomicLstig=false;
        filePath="../examples/flock.labs";
        verbose=false;
        spawn = Map.ofSeq [("Bird", 10)]
    }

    setPlaceholders <| Map.ofSeq [("bird", "10")]


    args 
    |> Result.map (fun x -> x.filePath)
    >>= readFile
    >>= parse
    |> (fun x -> printf "%A" x; x)
    |> log "Parse successful"
    //>>= uniqueDefs
    //|> log "All definitions are unique"
    >>= checkNames
    |> log "All names are defined"

    //|> log "Init valid"
    >>= checkComponents
    |> log "All components are valid"
    >+> (args |> Result.map (fun x -> x.spawn))
    >>= encode
    >>= translateHeader 
    >>= translateAll 
    |> logErr // Log any error at the end
    |> setReturnCode
