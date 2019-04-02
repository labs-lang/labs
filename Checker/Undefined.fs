module Checker.Undefined
open FSharpPlus.Lens

open Checker
open LabsCore
open Types


let checkProcessNames t =
    let checkAgent (a:SymbolTable) (table: SymbolTable) =
        let definitions = Map.union a.processes table.processes
        let isDefined n = Map.containsKey n definitions
        
        definitions
        |> Map.mapValues (fun x -> Process.usedNames x.proc)
        |> Map.values
        |> Set.unionMany
        |> Set.toList
        
        |> List.groupBy fst
        |> List.filter (fst >> isDefined >> not)
        |> List.map (fun (name, l) -> {ErrorMessage.what=UndefProcess name; where=(List.map snd l)})
        |> wrap table []
    
    trFold checkAgent (t.children |> Map.values) t



//TODO
//let undefBehavior (agent:Agent) =
//    let hasBehavior =
//        agent.processes
//        |> List.map (fun x -> x.name)
//        |> List.exists ((=) "Behavior")
//    [],
//        if (not hasBehavior)
//        then [{ErrorMessage.what=UndefBehavior(agent.name); where=[agent.pos]}]
//        else []
//        
//let undefAgent (ast: Ast) =
//    let sys, _, agents, _ = ast
//    let noSuchAgent (pos, name, _) = 
//        [],
//            if List.forall (fun (agent:Agent) -> agent.name <> name) agents
//            then [{ErrorMessage.what=UndefAgent(name); where=[pos]}]
//            else []
//    fold noSuchAgent sys.spawn

//let undefProcess (ast: Ast) =
//    let sys, _, agents, _ = ast
//    let names p = 
//        let base_ b =
//            match b.stmt with
//            | Name s -> Set.singleton b
//            | _ -> Set.empty
//        Process.cata base_ (fun _ _ -> id) (fun _ -> Set.unionMany) p
//            
//        0 // TODO
//    0 //TODO