module StepResult 
open FParsec

type StepResult<'T> =
    | Continue of 'T * string
    | Fail of string
    //static member (>>=) (f: 'T -> StepResult<'U>, m) =
        //match m with
        //| Ok(a, str) -> f a
        //| Fail(s) -> Fail(s)

let (>>=) r (f: 'T -> StepResult<'U>) = 
    match r with
    | Continue(a, str) -> f a
    | Fail(s) -> Fail(s)

let log fn x = 
    let step = fn x
    match step with
    | Continue(_, s) -> printfn "%s" s
    | Fail(s) -> printfn "Error: %s" s
    step

let parse text =
     match CharParsers.run Parser.full text with
     | Success(processes, _, _) ->
        Continue(processes, sprintf "Parsing successful:\n %A" processes)
     | Failure(errorMsg, _, _) -> 
        Fail(sprintf "Parsing failed:\n %s" errorMsg)
