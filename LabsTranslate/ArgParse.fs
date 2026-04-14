module internal LabsTranslate.ArgParse
open Argu
open Frontend.Message
open TranslationKit

type Arguments =
    | [<Mandatory>] [<Unique>] File of path:string
    | [<Unique>] Values of string list
    // Emulation program parameters
    | [<Mandatory>] [<Unique>] Bound of int
    | [<Unique>] Enc of EncodeTo
    | [<Unique>] Fair of Fairness
    | Simulation
    | [<Unique>] Sync
    // Information dumps
    | [<Unique>] Info
    | [<Unique>]  Ast
    // Specify which property to analyze
    | Property of string
    | No_Properties
    // C translation parameters
    | [<Unique>] No_Bitvector
    | [<Unique>] No_Bitwise
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Ast -> "dump AST and quit."
            | File _ -> "specify a file."
            | Info -> "dump information on the system and quit."
            | No_Bitvector -> "disable bitvector optimizations"
            | No_Bitwise -> "use logical and/or instead of bitwise. Implies --no-bitvector"
            | Values _ -> "specify the value of placeholders (use the format key=value)."
            | Bound _ -> "specify the number of iterations (for bounded model checking)."
            | Sync -> "force synchronous sending of stigmergic messages"
            | Simulation -> "encode in simulation mode (default: verification mode)."
            | Fair _ -> "Specify fairness constraint. (default: none)"
            | Enc _ -> "specify the target encoding."
            | Property _ -> "specify the property to consider, others will be ignored."
            | No_Properties -> "ignore all properties."

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
        |> Seq.map (fun a -> (a[0], a[1]))
        |> Map.ofSeq
    try
        args.PostProcessResult (<@ Values @>, parseValues)
    with 
        e -> Map.empty
