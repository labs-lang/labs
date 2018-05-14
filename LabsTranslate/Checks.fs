module Checks
open Types
open Functions
open Base

let withcommas : seq<string> -> string = (String.concat ",")

/// Verifies that no process is defined more than once.
let uniqueDefs processes = 
    let duplicateProcesses = processes |> List.map fst |> duplicates
    if duplicateProcesses.Length = 0 then
        Result.Ok processes
    else
        duplicateProcesses
        |> sprintf "The following processes have multiple definitions: %A"
        |> Result.Error

//let uniqueKeys sysdef =
//    let duplicateKeys = (List.concat [sysdef.iface; sysdef.lstig; sysdef.environment])
//    //|> duplicates
//    0

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