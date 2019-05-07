module Frontend.Transform
open Checks
open Types
open SymbolTable
open Outcome
open Message
open LTS

// Duplicate attributes in different agents are legal.
let private envAndLstigVars sys lstigs =
    List.concat (List.map (fun x -> x.def.vars |> Set.unionMany |> Set.toList) lstigs)
    |> List.append sys.def.environment

let check (sys, lstigs, agents', properties) =
    let vars = envAndLstigVars sys lstigs
    
    let undefSpawned =
        sys.def.spawn
        |> List.filter (fun d -> not <| List.exists (fun (a:Node<_>) -> a.name = d.name) agents')
        |> List.map (fun d -> {what=UndefAgent d.name; where=[d.pos]})
        |> wrap () []
    
    zero ()
    (* check for duplicate definitions *)
    <??> dupNames sys.def.spawn
    <??> dupNames agents'
    <??> dupNames lstigs
    <??> dupNames sys.def.processes
    <??> dupNames vars
    <?> fold (checkAgent vars) agents'
    
    (* Check for undefined agents in spawn section *)
    <??> undefSpawned
    
let run externs (sys, lstigs, agents', properties) =
    let vars = envAndLstigVars sys lstigs
    let (agents: Node<Agent> list) =
        let spawned = List.map (fun (d: Node<_>) -> d.name) sys.def.spawn |> Set.ofList
        List.filter (fun a -> Set.contains a.def.name spawned) agents'
    
    zero (SymbolTable.empty)
    <??> check (sys, lstigs, agents', properties)
    (* map non-interface variables *)
    <~> fold (tryAddVar externs) vars
    <~> fun x -> fold mapVar (Map.values x.variables) x
    
    (* map attributes; add stigmergies, global processes, agents*)
    <~> fold (tryAddIface externs) agents
    <~> fold (tryAddStigmergy externs) lstigs
    <~> fold (tryAddProcess externs) sys.def.processes
    <~> fun x ->
        fold (tryAddAgent externs) agents (x, (Set.empty, (0, ExecPoint.empty, Map.empty, Map.empty)))
    <~> (fst >> zero)
    <~> (makeSpawnRanges externs) sys.def.spawn