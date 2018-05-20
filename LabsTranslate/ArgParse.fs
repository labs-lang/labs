module ArgParse
open Argu

type Arguments =
| [<Mandatory>] [<Unique>] File of path:string
| [<Mandatory>] [<Unique>] Bound of int
| Values of string list
 interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify a file."
            | Values _ -> "specify the value of placeholders (use the format key=value)."
            | Bound _ -> "specify the number of iterations (for bounded model checking)."

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
        let vals = parsed.PostProcessResult (<@ Values @>, parseValues)
        Result.Ok (parsed, vals)
    with e ->
        e.Message |> Result.Error

    