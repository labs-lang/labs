module internal Checks
open Types
open Link
open Base

/// Performs several checks related to components
let checkComponents sys =
    let undefBehaviors = 
        sys.components
        |> Map.filter (fun _ def -> not <| def.processes.ContainsKey "Behavior")
    if sys.components.IsEmpty then
        Result.Error "No agents defined"
    else
        if undefBehaviors.IsEmpty then
            Result.Ok ()
        else
            undefBehaviors
            |> Map.map (fun name _ -> sprintf "%s: Behavior is undefined" name)
            |> Map.values
            |> String.concat "\n"
            |> Result.Error



/// Verifies that all process names in the program have been defined.
let checkNames sys =
    let usedNames = 
        let basefn fn b = 
            match b with 
            | Name(s, _) -> Set.singleton b
            | Guard(_, p, _)
            | Paren(p, _) -> fn p
            | _ -> Set.empty
        walk basefn Set.unionMany Set.unionMany Set.unionMany
    
    let check (definitions: Map<string, 'a PROC>) =
        let formatErrorMsg (b: 'a BASE) =
            sprintf "%A Undefined process: %s" b.Pos (string b)
        let used = 
            (Map.values definitions)
            |> Seq.map usedNames
            |> Set.unionMany
        let undefs = 
            Set.filter
                (fun n -> not <| Map.containsKey (string n) definitions) used
        if undefs.IsEmpty then Ok()
        else 
            undefs
            |> Set.map formatErrorMsg
            |> String.concat "\n"
            |> Error

    let checkAgent (a:ComponentDef<_>) =
        check (Map.merge sys.processes a.processes)

    Map.mapValues checkAgent sys.components
    |> Map.values
    |> Seq.append (Seq.singleton (check sys.processes))
    |> Seq.reduceBack (<&&>)

let analyzeKeys sys = 
    let comps = Map.values sys.components

    let conflicts (seqOfVars:Set<Var> seq) =
        let groups =
            seqOfVars
            |> Set.unionMany
            |> List.ofSeq
            |> List.groupBy (fun x -> x.name)
            //|> Set.unionMany
            //|> Seq.map (fun k -> k, (seqOfVars |> Seq.choose (Map.tryFind k) |> List.ofSeq))
            |> Map.ofSeq

        groups
        |> Map.mapValues (fun l -> 
            l,
            if l.IsEmpty then false
            else List.exists (fun v -> v.vartype <> l.Head.vartype) l)
        |> Map.filter (fun _ -> snd)
        |> Map.keys
        |> fun x -> 
            if x.IsEmpty then seqOfVars else 
                x
                |> Set.map (sprintf "Variable %s cannot be both Scalar and Array") 
                |> String.concat "\n"
                |> failwith

    /// Makes a dictionary with information about each 
    /// variable in setOfVars.
    let makeInfo startFrom (setOfVars:Set<Var> seq) = 
        let update (mapping, nextIndex) (var:Var) =
            if Map.containsKey var.name mapping
            then mapping, nextIndex
            else 
                Map.add var.name nextIndex mapping,
                match var.vartype with
                | Scalar _ -> nextIndex + 1
                | Array n -> nextIndex + n

        setOfVars
        |> Seq.fold (fun x s -> Set.fold update x s) (Map.empty, startFrom)

    let attrKeys, maxI = 
        comps
        |> Seq.map (fun c -> c.iface)
        |> conflicts
        |> (makeInfo 0)

    let lstigkeys, maxL =
        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.fold 
            (fun (map, i) vars -> 
                let newInfo, newI = makeInfo i (seq vars)
                (Map.merge map newInfo, newI)
            )
            (Map.empty, 0)

    let envKeys, maxE = 
        sys.environment
        |> Seq.singleton
        |> makeInfo 0

    attrKeys
    |> Map.mergeIfDisjoint lstigkeys
    |> Map.mergeIfDisjoint envKeys
    |> Ok
    >+> Ok (max maxI 1, max maxL 1, max maxE 1)