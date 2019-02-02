module internal Base 
open FParsec
open System.IO
open Parser

type KeyMapping = Map<string, int>

/// Bind operator for Result.
#if DEBUG
let inline (>>=) r f = try Result.bind f r with ex -> Result.Error (ex.Message + ex.StackTrace)
#else
let inline (>>=) r f = try Result.bind f r with ex -> Result.Error (ex.Message)
#endif

// Returns r2 if r1 is Ok, else returns r1
let inline (<&&>) r1 r2 = r1 >>= (fun _ -> r2)

let setReturnCode r =
    match r with 
    | Result.Ok _ -> 0
    | Result.Error _ -> 10

/// Puts the results of r2 and r1 in a new Result
let (>+>) r2 r1 =
    match r2, r1 with
    | Result.Ok a, Result.Ok b -> Result.Ok (a, b)
    | Result.Error err, _
    | _, Result.Error err -> Result.Error err
    
let log msg r = 
    match r with
    | Result.Ok _ -> 
        eprintfn "\n%s" msg
        r
    | Result.Error _ -> r

let logErr = function
    | Result.Ok a -> Result.Ok a
    | Result.Error s -> 
        eprintfn "[Error] %s" s
        Result.Error ""

let readFile filepath =
    Result.Ok filepath
    >>= (Result.Ok << File.ReadAllText)

let withcommas x = (String.concat ", " x)

let parse (text, (placeholders:Map<string, string>)) =
    let checkPlaceholders s =
        (Map.keys placeholders)
        |> Set.difference s
        |> fun z -> 
            if (Set.isEmpty z)
            then Result.Ok s 
            else Result.Error(sprintf "Uninitialized external variable: %s" (withcommas z))

    let wrapParserResult p text = 
        try
            match CharParsers.run p text with
            | Success(a, _, _) -> Result.Ok a
            | Failure(errorMsg, _, _) -> 
                Result.Error (sprintf "Parsing failed:\n %s" errorMsg)
        with
            ex -> Result.Error ex.Message

    let stripped =
        CharParsers.run stripComments text
        |> function | Success(a, _, _) -> a | Failure(msg,_,_) -> failwith msg

    let defPlaceholders = 
        stripped
        |> (wrapParserResult pre)
        |> Result.map (Set.filter ((<>) ""))
        >>= checkPlaceholders
        >+> (wrapParserResult allPlaceholders stripped |> Result.map Set.ofList |> Result.map (Set.filter ((<>) "")))
        >>= fun (def, all) -> 
            let diff = (Set.difference all def)
            if diff.IsEmpty
            then Result.Ok def
            else
                diff
                |> Set.map ((+) "_")
                |> withcommas
                |> sprintf "External variables %s have not been defined in the 'extern' section." 
                |> Result.Error

    defPlaceholders
    |> Result.map ((Set.fold (fun (txt:string) ph -> txt.Replace("_"+ph, placeholders.[ph])) stripped))
    >>= (wrapParserResult parse)

let enumerate s = 
    s
    |> Seq.mapi (fun i x -> x, i)
    |> Map.ofSeq


let makeCounter (start: int) =
    let x = ref start
    let incr() =
        x := !x + 1
        !x
    incr

