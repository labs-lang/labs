module Checks
open Types
open Base

let withcommas : seq<string> -> string = (String.concat ",")

/// Verifies that no process is defined more than once.
let uniqueDefs processes = 
    let definedNames = processes |> List.map fst
    let duplicates = 
        definedNames 
        |> List.groupBy id
        |> List.filter (fun (x, y) -> y.Length > 1)
        |> List.map fst
    if duplicates.Length = 0 then
        Result.Ok processes
    else
        Result.Error (sprintf "The following processes have multiple definitions: %A" duplicates)

/// Verifies that all process names in the program have been defined.
let checkNames processes =
    let definedNames = fstSet processes

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

    if undefinedNames.Count = 0 then Result.Ok processes
    else Result.Error (sprintf "The following processes are undefined: %s" (withcommas undefinedNames))

///Checks that the Init oricess (if any) is a sequence of environment writes





let checkComponents ((sys:SystemDef), processes) =
    if sys.components.IsEmpty then Result.Error ("No components defined")
    else
        let names = fstSet processes
        let allDefined = 
            sys.components
            |> List.forall names.Contains
        if allDefined then
            Result.Ok (sys, processes)
        else
            // Todo 
            Result.Error("Some components are invalid")