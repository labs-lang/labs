module Checks
open Types
open Base

let withcommas : seq<string> -> string = (String.concat ", ")

/// Verifies that all process names in the program have been defined.
let checkNames sys =
    let globalNames = Map.keys sys.processes

    let rec usedNames = function
    | Name(n) -> Set.singleton n
    | Await(_, p) -> usedNames p
    | Seq(p, q)
    | Par(p, q)
    | Choice(p, q) -> Set.union (usedNames p) (usedNames q)
    | _ -> Set.empty

    let undefinedNames processes defNames = 
        processes
        |> Map.values
        |> Seq.map usedNames
        |> Set.unionMany
        |> (fun x -> Set.difference x defNames)

    let checkComps = 
        sys.components
        |> Map.map (fun _ x -> 
            undefinedNames x.processes (Set.union globalNames (Map.keys x.processes)))
        |> Map.filter (fun _ undef -> undef.Count > 0)
    let globalUndef = undefinedNames sys.processes globalNames    

    let rec makeMsg globalCount localCount = 
        match globalCount, localCount with
        | (true,true) -> ""
        | (_,true) -> sprintf "global: the following processes are undefined: %s" (withcommas globalUndef)
        | (true,_) -> 
            checkComps
            |> Map.map (fun name undefs ->
                sprintf "%s: the following processes are undefined: %s" name (withcommas undefs))
            |> Map.values
            |> String.concat "\n"
        | (_,_) -> (makeMsg  true false) + "\n" + (makeMsg false true)

    let msg = makeMsg globalUndef.IsEmpty checkComps.IsEmpty
    if msg = "" then Result.Ok sys
    else Result.Error (msg)

let checkComponents sys =
    let isDefined (def:ComponentDef) name  =
        sys.processes.ContainsKey name || def.processes.ContainsKey name

    let undefBehaviors = 
        sys.components
        |> Map.filter (fun _ def -> not <| isDefined def def.behavior)

    if sys.components.IsEmpty then Result.Error ("No components defined")
    else if undefBehaviors.IsEmpty then
        Result.Ok sys
    else
        let x = 
            undefBehaviors
            |> Map.map (fun _ def -> def.behavior)
            |> Map.map (sprintf "%s: Behavior is undefined: %s")
            |> Map.values
            |> withcommas
            |> (fun s -> Result.Error(s))
        x
