module Checks
open Types
open Base
open Templates


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
    else Result.Error msg

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
        undefBehaviors
        |> Map.map (fun _ def -> def.behavior)
        |> Map.map (sprintf "%s: Behavior is undefined: %s")
        |> Map.values
        |> withcommas
        |> Result.Error

let rec checkKeysExpr = function
| K(k) -> k |> Set.singleton
| Arithm(e1,_,e2) -> Set.union (checkKeysExpr e1) (checkKeysExpr e2)
| Const(_) -> Set.empty

let rec checkKeys (procs:Map<string,Process>) (names:Set<string>) = 
    function
    | Nil
    | Skip -> Set.empty
    | Base(a) -> 
        match a with
        | AttrUpdate(k,e)
        | LStigUpdate(k,e)
        | EnvWrite(k,e) -> (checkKeysExpr e).Add(k)
        | EnvRead(k1,k2) -> Set [k1;k2]
    | Await(_, a) -> checkKeys procs names a
    | Seq(a,b) | Choice(a,b) | Par(a,b) -> Set.union (checkKeys procs names a) (checkKeys procs names b)
    // Only visit a named process if it has not been visited yet
    | Name(s) when (not <| names.Contains s) -> 
        checkKeys procs (names.Add s) procs.[s]
    | Name(s) -> Set.empty

let analyzeKeys sys = 
    let initToType = function
    | ChooseI(_) | RangeI(_) -> Int(0)
    | ChooseP(_) | RangeP(_) -> P(0,0)

    let comps = Map.values sys.components
    let attrKeys = 
        comps
        |> Seq.map (fun c -> c.iface)
        |> Seq.map (Map.mapi (fun i k init -> {index=i; location=I; typ=(initToType init)}))
        |> Seq.fold (fun result m -> Map.merge result m) Map.empty
    let maxindex = 
        attrKeys
        |> Map.values
        |> Seq.maxBy (fun info -> info.index)
        |> (fun x -> x.index + 1)
    eprintfn "%A" attrKeys
    let lstigKeys = 
        comps
        |> Seq.map (fun c -> c.lstig)
        |> Seq.map (Map.mapiFrom maxindex (fun i k init -> {index=i; location=L; typ=(initToType init)}))
        |> Seq.fold (fun result m -> Map.merge result m) Map.empty
        
    eprintfn "%A" lstigKeys

    let mapping = Map.merge attrKeys lstigKeys
    // TODO add environment

    //toJson sys.spawn mapping types
    // TODO add key check

    Result.Ok (mapping)