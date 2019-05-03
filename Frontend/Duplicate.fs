module Checker.Duplicate
open System
open FSharpPlus
open FSharpPlus.Lens
open Checker
open FParsec
open Types

/// Checks if a list of INodes contains duplicate identifiers
//let dupNames lst =
//    let pos_ (x:INode) = x.Pos
//    lst
//    |> List.map (fun x -> x :> INode)
//    |> List.groupBy (fun x -> x.Name)
//    |> List.filter (fun x -> List.length (snd x) > 1)
//    |> List.map (fun (name, defs) -> { ErrorMessage.what=Duplicate name; where=List.map pos_ defs } )
//    |> fun x -> [], x
//    
//let inline dupList lst =
//    lst
//    |> List.groupBy (view _2)
//    |> List.filter (fun x -> List.length (snd x) > 1) 
//    |> List.map (fun (name, l) -> {ErrorMessage.what=Duplicate name; where=List.map (view _1) l } )
//    |> fun x -> [], x

//let dupProcessDef (ast: Ast) =
//    let sys, _, agents, _ = ast
//    let agent_ agent =
//        let agentdefs = agent.processes |> List.map (view _2) |> Set.ofList
//        // Global processes are shadowed by local ones
//        List.filter (view _2 >> fun x -> not (Set.contains x agentdefs))  sys.processes
//        |> List.append agent.processes
//        |> dupList
//    fold agent_ agents