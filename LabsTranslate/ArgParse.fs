module internal LabsTranslate.ArgParse
open Argu
open Frontend.Message
open Encode

type Arguments =
    | [<Mandatory>] [<Unique>] File of path:string
    | [<Mandatory>] [<Unique>] Bound of int
    | Enc of EncodeTo
    | Fair
    | No_Bitvector
    | Sync
    | Info
    | Simulation
    | [<Unique>] Values of string list
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify a file."
            | Info _ -> "do not translate, only gather information on the system"
            | No_Bitvector _ -> "disable bitvector optimizations"
            | Values _ -> "specify the value of placeholders (use the format key=value)."
            | Bound _ -> "specify the number of iterations (for bounded model checking)."
            | Sync _ -> "force syncronous sending of stigmergic messages"
            | Simulation _ -> "encode in simulation mode (default: verification mode)."
            | Fair -> "enforce fair interleaving of components."
            | Enc _ -> "specify the target encoding."

let argParser = ArgumentParser.Create<Arguments>(programName = "LabsTranslate")

let parseCLI argv =
    try
        argParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    with e ->
        raise (LabsException {What = CLI e.Message; Where=[]})

let getExterns (args:ParseResults<_>) = 
    let parseValues (vals:string list) =
        vals
        |> Seq.map (fun x -> x.Split "=")
        |> Seq.filter (fun a -> Array.length a = 2)
        |> Seq.map (fun a -> (a.[0], a.[1]))
        |> Map.ofSeq
    try
        args.PostProcessResult (<@ Values @>, parseValues)
    with 
        e -> Map.empty
