module Frontend.Frontend
open Frontend.Checks
open Frontend.SymbolTable
open Frontend.Message
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
