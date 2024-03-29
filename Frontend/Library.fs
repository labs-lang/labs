﻿module Frontend.Frontend
open Frontend.Checks
open Frontend.SymbolTable
open Frontend.Message
open LabsCore
open LabsCore.ExprTypes
open LabsCore.Grammar
open Frontend.Outcome
open Frontend.STS

// Duplicate attributes in different agents are legal.
let private envAndLstigVars sys lstigs =
    List.collect (fun x -> x.Def.Vars |> Set.unionMany |> Set.toList) lstigs
    |> List.append sys.Def.Environment

let check (sys, lstigs, agents', _) =
    let vars = envAndLstigVars sys lstigs
    
    let undefSpawned =
        sys.Def.Spawn
        |> List.filter (fun d -> not <| List.exists (fun (a:Node<_>) -> a.Name = d.Name) agents')
        |> List.map (fun d -> {What=UndefAgent d.Name; Where=[d.Pos]})
        |> wrap () []
    
    zero ()
    (* check for duplicate definitions *)
    <??> dupNames sys.Def.Spawn
    <??> dupNames agents'
    <??> dupNames lstigs
    <??> dupNames sys.Def.Processes
    <??> dupNames vars
    <?> fold (checkAgent vars) agents'
    
    (* Check for undefined agents in spawn section *)
    <??> undefSpawned
    
let run externs (sys, lstigs, agents', assume, properties) =
    let vars = envAndLstigVars sys lstigs
    let (agents: Node<Agent> list) =
        let spawned = List.map (fun (d: Node<_>) -> d.Name) sys.Def.Spawn |> Set.ofList
        List.filter (fun a -> Set.contains a.Def.Name spawned) agents'
    
    zero Frontend.SymbolTable.empty
    <~> (fun x -> zero {x with Externs=externs})
    <??> check (sys, lstigs, agents', properties)
    (* map non-interface variables *)
    <~> fold (tryAddVar externs) vars
    <~> fun x -> fold mapVar (Map.values x.Variables |> Seq.filter isEnvVar) x
    <~> fun x ->
            (* Ensure that variables in the same tuple get contiguous indices *)
            Map.values x.Variables
            |> Seq.filter isLstigVar
            |> Seq.groupBy (fun v -> v.Location)
            |> Seq.map snd
            |> Seq.fold (fun x' s -> x' <~> fold mapVar s) (zero x)
    
    (* map attributes; add stigmergies, global processes, agents*)
    <~> fold (tryAddIface externs) agents
    <~> fold (tryAddStigmergy externs) lstigs
    <~> fold (tryAddProcess externs) sys.Def.Processes
    <~> (makeSpawnRanges externs) sys.Def.Spawn
    <~> fun x ->
        fold (tryAddAgent externs) agents (x, (Set.empty, (0, ExecPoint.empty, Map.empty, Map.empty)))
    <~> (fst >> zero)
    (* properties can only be added after spawn *)
    <~> fold (tryAddProperty externs) properties
    <~> fold (tryAddAssume externs) (assume |> Option.defaultValue [])

/// Turns a variable initializer into a list of BExpr
/// (multiple BExprs are returned when v is an array).
let initBExprs idfn (v:Var<_>, i: int) =
    let mapFn r =
        let leafFn l = match l with | Id _ -> idfn | _ -> l
        Expr.map leafFn (fun _ o of_ -> {r with Offset=o; OfAgent=of_})
    let refs =
        let r = {Var=(v, i); Offset = None; OfAgent = None}
        match v.Vartype with
        | Scalar -> [r]
        | Array s ->
            let indexes = [for i in 0..s.Length-1 -> [0..s[i]-1] ]
            // Kudos to https://stackoverflow.com/a/3334871c for this cartesian product function
            let rec cart1 LL = 
                match LL with
                | [] -> Seq.singleton []
                | hd::Ls -> seq {for x in hd do for xs in cart1 Ls -> x::xs}
            let allIndexes = cart1 indexes |> Seq.toList |> List.map (List.map (Leaf << Const))
            let makeOneRef ind = {r with Offset = Some ind}
            List.map makeOneRef allIndexes 
            
    match v.Init with
    | Undef -> List.map (fun r -> Compare(Ref r, Equal, Leaf(Extern "undef_value"))) refs
    | Choose l ->
        let choice r =
            List.map (mapFn r >> (fun e -> Compare(Ref r, Equal, e))) l
            |> fun l -> Compound(Disj, l)
        List.map choice refs
    | Range (start_, end_) -> 
        let between r = Compound(Conj, [Compare(Ref r, Geq, mapFn r start_); Compare(Ref r, Less, mapFn r end_)])
        List.map between refs
