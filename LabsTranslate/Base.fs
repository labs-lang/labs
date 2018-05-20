module Base 
open FParsec
open System.IO

type TypeofKey = | I | L | E

type KeyMapping = Map<(string * TypeofKey),int>

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
        f a >>= (fun x -> Result.Ok(x,b))
    | Result.Error(_) -> r  

// Keeps the first element and binds the second
let (>>=.) r f =
    match r with
    | Result.Ok(a,b) -> 
        f b >>= (fun x -> Result.Ok(a,x))
    | Result.Error(_) -> r

///// Puts the result of r2 and that of r1 in a new Result
let (>+>) r2 r1 =
    match r2, r1 with
    | Result.Ok(a), Result.Ok(b) -> Result.Ok((a, b))
    | Result.Error(err), _ -> Result.Error(err)
    | _, Result.Error(err) -> Result.Error(err)

let (<&>) f g a =
    (f a) >+> (g a)

/// Binds r as the first argument of f.
let (>>?) r f =
    match r with
    | Result.Ok(a) -> f a
    | Result.Error(err) -> (fun _ -> Result.Error(err))

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

let parse text =
    try
        match CharParsers.run Parser.parse text with
        | Success(sys, _, _) ->
        Result.Ok(sys)
        | Failure(errorMsg, _, _) -> 
            Result.Error(sprintf "Parsing failed:\n %s" errorMsg)
    with
        ex -> Result.Error(ex.Message)