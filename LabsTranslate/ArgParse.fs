module ArgParse
open Argu

type Arguments =
| [<Mandatory>] [<Unique>] File of path:string
| [<Mandatory>] [<Unique>] Bound of int
| Fair
| Info
| [<Unique>] Values of string list
 interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify a file."
            | Info _ -> "do not translate, only gather information on the system"
            | Values _ -> "specify the value of placeholders (use the format key=value)."
            | Bound _ -> "specify the number of iterations (for bounded model checking)."
            | Fair -> "enforce fair interleaving of components."

let argParser = ArgumentParser.Create<Arguments>(programName = "LabsTranslate")

let parseValues (vals:string list) =
    vals
    |> Seq.map (fun x -> x.Split "=")
    |> Seq.filter (fun a -> Array.length a = 2)
    |> Seq.map (fun a -> (a.[0], a.[1]))
    |> Map.ofSeq

let parseCLI argv =
    try
        let parsed = argParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        Result.Ok parsed
    with e ->
        e.Message |> Result.Error

let bound result = 
    Result.map (fun (args:ParseResults<_>) -> args.GetResult <@ Bound @>) result

let placeholders args = 
    try
        args
        |> Result.map (fun (args:ParseResults<_>) -> args.PostProcessResult (<@ Values @>, parseValues))
    with 
        e -> Result.Ok Map.empty

let filenameOf result = 
    Result.map (fun (args:ParseResults<_>) -> args.GetResult <@ File @>) result
