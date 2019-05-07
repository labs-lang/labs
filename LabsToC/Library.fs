module LabsToC.LabsToC

open Frontend.LTS
open Frontend
open Expressions
open Types
open Tokens
open LabsCore
open FSharpPlus
open Frontend.Liquid

let private goto = parse "templates/goto.c"

let encodeAgent sync table (a:AgentTable) =
    
    let encodeTransition (t:Transition) =
        let funcName =
            Map.map (sprintf "_%i_%i") t.entry |> Map.values
            |> String.concat ""
            |> (+) (if t.last then "_last" else "")
        let guards = table.guards.TryFind t.action |> Option.defaultValue Set.empty
        let assignments = t.action.def |> (function Act a -> Some a | _ -> None)
        
        /// Set of keys that the agent will have to confirm
        let qrykeys =
            let getLstigVarsBExpr =
                let compare_ _ e1 e2 = Set.union (getLstigVars e1) (getLstigVars e2)
                BExpr.cata (fun _ -> Set.empty) id compare_ (fun _ -> Set.union)
            assignments
            |>> (fun a -> List.map (getLstigVars << snd) a.updates)
            |>> Set.unionMany
            |> Option.orElse (Some Set.empty)
            |>> Set.union (guards |> Set.map getLstigVarsBExpr |> Set.unionMany)
            |>> Seq.map (Int << snd)
            |> Option.defaultValue (Seq.empty)
            |> Lst
        
        let liquidAssignment (k:Ref<Var<int>*int, unit>, expr) =
            let size = match (fst k.var).vartype with Array s -> s | _ -> 0
            Dict [
                "key",  Int (snd k.var)
                "offset",
                    k.offset |>> (procExpr.ExprTranslator >> Str) |> Option.defaultValue (Int 0)
                "size", Int size
                "expr", procExpr.ExprTranslator expr |> Str
            ]
        
        [
            "label", funcName |> Str
            "last", t.last |> Bool
            "siblings", t.siblings |> Seq.map Int |> Lst
            "entrycond", liquidPcs (t.entry |> Map.mapValues Set.singleton) |> Lst
            "exitcond", liquidPcs (t.exit) |> Lst
            "guards",
                guards
                |> Set.map (procExpr.BExprTranslator)
                |> Seq.map Str
                |> Lst
                
            "labs",
                string t.action.def
                |> (+) (if guards.IsEmpty then "" else ((guards |> Set.map string |> String.concat " and ") + tGUARD)) 
                |> Str
            "type",
                assignments
                |>> fun a -> a.actionType
                |>> function | I -> "attr" | L _ -> "lstig" | E -> "env"
                |> Option.defaultValue ""
                |> Str
            "qrykeys", qrykeys
            "sync", sync |> Bool
            "assignments", assignments
                |>> fun a -> a.updates
                |>> Seq.map liquidAssignment
                |> Option.defaultValue Seq.empty
                |> Lst         
        ]
        |> fun x -> (strRender x) goto
        
    Set.map (encodeTransition) a.lts