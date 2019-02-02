﻿module internal ArgParse
open Argu

type Arguments =
    | [<Mandatory>] [<Unique>] File of path:string
    | [<Mandatory>] [<Unique>] Bound of int
    | Fair
    | No_Bitvector 
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
            | Simulation _ -> "encode in simulation mode (default: verification mode)."
            | Fair -> "enforce fair interleaving of components."

let argParser = ArgumentParser.Create<Arguments>(programName = "LabsTranslate")

let parseCLI argv =
    try
        argParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    with e ->
        raise e

let placeholders (args:ParseResults<_>) = 
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
