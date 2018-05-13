module Base 
open FParsec
open System.IO

/// Returns a set of all the first values in a sequence of pairs.
let fstSet s = Set.ofSeq << (Seq.map fst) <| s

///Bind operator
let (>>=) r f = Result.bind f r

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

// Like bind, but keep the input value in the result
let (>>+) r f =
    match r with
    | Result.Ok(a) -> 
        f a >>= (fun x -> Result.Ok(a, x))
    | Result.Error(err) -> Result.Error(err)

let log msg result = 
    match result with
    | Result.Ok(_) -> 
        printfn "%s" msg
        result
    | Result.Error(s) -> result//printfn "Error: %s" (s.ToString())


let logErr result = 
    match result with
    | Result.Ok(_) -> result
    | Result.Error(s) -> 
        printfn "Error: %s" (s.ToString())
        result

let readFile filepath =
    try
        Result.Ok(File.ReadAllText filepath)
    with
    | ex -> Result.Error(ex.Message)

let parse text =
     match CharParsers.run Parser.parse text with
     | Success((procs, components), _, _) ->
        Result.Ok(procs, components)
     | Failure(errorMsg, _, _) -> 
        Result.Error(sprintf "Parsing failed:\n %s" errorMsg)
