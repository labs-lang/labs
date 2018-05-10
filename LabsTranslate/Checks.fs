module Checks
open Labs.Types
open StepResult
open FParsec

/// Verifies that no process is defined more than once.
let uniqueDefs processes = 
    let definedNames = processes |> List.map fst
    let duplicates = 
        definedNames 
        |> List.groupBy id
        |> List.filter (fun (x, y) -> y.Length > 1)
        |> List.map fst
    if duplicates.Length = 0 then
        Continue(processes, "All definitions are unique")
    else
        Fail(sprintf "The following processes have multiple definitions: %A" duplicates)

/// Verifies that all process names in the program have been defined.
let checkNames processes =
    let definedNames = processes |> List.map fst |> Set.ofList

    let rec usedNames = function
    | Name(n) -> Set.singleton n
    | Await(_, p) -> usedNames p
    | Seq(p, q)
    | Par(p, q)
    | Choice(p, q) -> Set.union (usedNames p) (usedNames q)
    | _ -> Set.empty

    let undefinedNames = 
        processes
        |> List.map snd
        |> List.map usedNames
        |> Set.unionMany
        |> (fun x -> Set.difference x definedNames)

    if undefinedNames.Count = 0 then Continue(processes, "All names are defined")  else 
    Fail(sprintf "The following processes are undefined: %A" undefinedNames)

let checkInit (processes: (string * Process) list) = 

    let rec checkExpr = function
    | Const(_) -> Set.empty
    | I(x) -> sprintf "I[%s]" x |> Set.singleton
    | L(x) -> sprintf "L[%A]" x |> Set.singleton
    | Sum(e1, e2) -> Set.union (checkExpr e1) (checkExpr e2)

    let rec check = function
    | Base(EnvWrite(k, e)) -> checkExpr e
    | Base(a) -> a.ToString() |> Set.singleton
    | Seq(p, q) -> Set.union (check p) (check q)
    | Par(p, q) -> checkWith "|" p q
    | Choice(p, q) -> checkWith "+" p q
    | Await(_,p) -> (check p).Add "->"
    | Name(a) -> Set.singleton a
    | Nil 
    | Tick -> Set.empty
    and checkWith str p q =
        check p
        |> Set.union (check q)
        |> Set.add str

    let resultOf p =
        let messages = check p
        if messages.Count = 0 then Continue(processes, "Init ok")
        else Fail("Invalid constructs in Init proces: " + String.concat ", " messages)

    processes
    |> List.tryFind(fun x -> (fst x) = "Init")
    |> Option.map snd
    |> Option.map resultOf
    |> Option.defaultValue (StepResult.Continue(processes, "Init ok (No Init process)"))
