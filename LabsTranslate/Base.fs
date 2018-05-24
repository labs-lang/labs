module Base 
open FParsec
open Types
open System.IO
open Parser

type TypeofKey = | I | L | E
type KeyInfo = {index:int; location:TypeofKey}
type KeyMapping = Map<string, KeyInfo>

let getInfoOrFail (m:KeyMapping) k = 
    match m.TryFind k with
    | Some(info) -> info
    | None -> failwith (sprintf "Unexpected key: %s" k)


///Bind operator
let (>>=) r f = try Result.bind f r with ex -> Result.Error ex.Message

let setReturnCode r =
    match r with 
    | Result.Ok(_) -> exit 0
    | Result.Error(_) -> exit 10

// Binds the first element and keeps the second
let (.>>=) r f =
    match r with
    | Result.Ok(a,b) -> 
        f a >>= fun x -> Result.Ok (x,b)
    | Result.Error(_) -> r  

// Keeps the first element and binds the second
let (>>=.) r f =
    match r with
    | Result.Ok(a,b) -> 
        f b >>= fun x -> Result.Ok(a,x)
    | Result.Error(_) -> r

///// Puts the result of r2 and that of r1 in a new Result
let (>+>) r2 r1 =
    match r2, r1 with
    | Result.Ok(a), Result.Ok(b) -> Result.Ok (a, b)
    | Result.Error(err), _ -> Result.Error err
    | _, Result.Error(err) -> Result.Error err

let (<&>) f g a =
    (f a) >+> (g a)


let log msg r = 
    match r with
    | Result.Ok(_) -> 
        eprintfn "\n%s" msg
        r
    | Result.Error(_) -> r

let logErr result = 
    match result with
    | Result.Ok(_) -> result
    | Result.Error(s) -> 
        eprintfn "\n%s" (s)
        Result.Error("")

let readFile filepath =
    try
        Result.Ok(File.ReadAllText filepath)
    with
    | ex -> Result.Error(ex.Message)

let wrapParserResult p text = 
    try
        match CharParsers.run p text with
        | Success(a, _, _) -> Result.Ok(a)
        | Failure(errorMsg, _, _) -> 
            Result.Error (sprintf "Parsing failed:\n %s" errorMsg)
    with
        ex -> Result.Error ex.Message

let withcommas x = (String.concat ", " x)


let parse (text, (placeholders:Map<string, string>)) =
    let checkPlaceholders s =
        (Map.keys placeholders)
        |> Set.difference s
        |> fun z -> 
            if (Set.isEmpty z) then Result.Ok(s) 
            else Result.Error(sprintf "Undefined placeholders: %s" (withcommas z))

    let foundPlaceholders = 
        wrapParserResult pre text
        |> Result.map Set.ofList
        |> Result.map (Set.filter ((<>) ""))
        >>= checkPlaceholders

    foundPlaceholders
    |> Result.map ((Set.fold (fun (txt:string) ph -> txt.Replace("&"+ph,placeholders.[ph])) text))
    |> Result.bind (wrapParserResult stripComments)
    |> Result.map (fun s -> eprintfn "%s" s; s)
    >>= (wrapParserResult parse)

let enumerate s = 
    s
    |> Seq.mapi (fun i x -> x,i)
    |> Map.ofSeq


let makeCounter (start: int) =
    let x = ref start
    let incr() =
        x := !x + 1
        !x
    incr

let findIndex comparison typeofkey mapping =
    mapping
    |> Map.filter (fun _ info -> info.location = typeofkey)
    |> Map.fold (fun state k (info) -> comparison state info.index) 0

let findMaxIndex mapping = findIndex max mapping
let findMinIndex mapping = findIndex min mapping