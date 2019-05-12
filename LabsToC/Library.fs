module LabsToC.LabsToC

open Frontend.LTS
open Frontend
open Expressions
open Types
open Tokens
open LabsCore
open FSharpPlus
open Frontend.Liquid
open LabsCore
open Outcome
open Types

let private goto = parse "templates/goto.c"
let private init = parse "templates/init.c"
let private header = parse "templates/header.c"
let private UNDEF = -128 (*sentinel value for undef*)

let encodeHeader bound isSimulation noBitvectors (table:SymbolTable) =
    let fromTo (v:Var<_>) =
        snd table.m.[v.name], snd table.m.[v.name] + (match v.vartype with Scalar -> 0 | Array i -> i) 
    let tupleStart, tupleEnd, maxTuple = //TODO maybe move to frontend
        table.variables
        |> Map.filter (fun _ -> isLstigVar)
        |> Map.values
        |> Seq.groupBy (fun v -> v.location)
        |> Seq.map(fun (_, s) -> (Seq.map fromTo s))
        |> fun s -> Seq.map (fst << Seq.minBy fst) s, Seq.map (snd << Seq.maxBy snd) s
        |> fun (s1, s2) -> s1, s2, (Seq.zip s1 s2 |> Seq.map (fun (a, b) -> b - a + 1) |> Seq.max)
    
    let getTypedef num = 
        let getStandardTypes = 
            function
            | a, b when a >= 0     && b < 256      -> "unsigned char"
            | a, b when a > -128   && b < 128      -> "char"
            | a, b when a >= 0     && b < 65536    -> "unsigned short"
            | a, b when a > -32768 && b < 32768    -> "short"
            | a, _ when a >= 0                     -> "unsigned int"
            | _ -> "int "
        let bitwidth num = 
            System.Math.Log(float num, 2.) |> int |> (+) 1
        if noBitvectors
        then getStandardTypes (0, num)
        else sprintf "unsigned __CPROVER_bitvector[%i]" (bitwidth num)
    
    let maxpc =
        Map.mapValues (fun (x:AgentTable) -> Map.keys x.init) table.agents
        |> Map.values |> Seq.concat |> Seq.max
    
    let defines = 
        [
            "BOUND", bound
            "MAXCOMPONENTS", table.spawn |> Map.values |> Seq.map snd |> Seq.max
            "MAXPC", maxpc + 1 
            "MAXKEYI", max table.m.nextI 1
            "MAXKEYL", max table.m.nextL 1
            "MAXKEYE", max table.m.nextE 1
            "MAXTUPLE", maxTuple
        ]
        |> (fun l -> if isSimulation then ("SIMULATION", 1)::l else l)
        |> Map.ofList
    
    let typedefs =
        [
            "TYPEOFVALUES", "short"
            "TYPEOFPC", "unsigned char"
            "TYPEOFTIME", "unsigned char" 
            "TYPEOFAGENTID", getTypedef defines.["MAXCOMPONENTS"]
            "TYPEOFKEYIID", getTypedef defines.["MAXKEYI"]
            "TYPEOFKEYLID", getTypedef defines.["MAXKEYL"]
            "TYPEOFKEYEID", getTypedef defines.["MAXKEYE"]
            "Bool", getTypedef 1
        ]
    
    let defines' = defines |> Map.map (fun a b -> Dict ["name", Str a; "value", Int b]) |> Map.values
    eprintfn "%A" defines'
    zero defines'

let encodeInit (table:SymbolTable) =
    let lstigvars agentName =
        table.variables
        |> Map.filter (fun _ v -> match v.location with | L (s, _) when table.agents.[agentName].lstig.Contains s -> true | _ -> false)
        |> Map.values
        |> Seq.sortBy (fun v -> snd table.m.[v.name])
    
    let env =
        table.variables
        |> Map.filter (fun _ -> isEnvVar)
        |> Map.values
        |> Seq.sortBy (fun v -> snd table.m.[v.name])
        |> Seq.map (fun v ->
                Frontend.Transform.initBExprs UNDEF Expr.evalCexprNoId table.m.[v.name]
                |> List.map (fun b -> (initExpr "").BExprTranslator b false |> Str)
            )
        |> Seq.concat
    
    let vars =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
            table.agents.[name].variables
            |> List.append (lstigvars name |> List.ofSeq)
            |> List.map (fun v tid ->
                Frontend.Transform.initBExprs UNDEF (Expr.evalConstExpr (fun _ -> tid)) table.m.[v.name]
                |> List.map (fun b -> (initExpr (string tid)).BExprTranslator b false))
                |> List.map (fun x i -> List.map Str (x i))
            |> List.map (fun f -> List.map f [_start.._end-1])
            |> List.concat |> List.concat)
        |> Map.values
        |> List.concat
        
    let tstamps =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
                (lstigvars name)
                |> Seq.map (fun v tid -> Dict ["tid", Int tid; "index", Int (snd table.m.[v.name])])
                |> Seq.map (fun f -> List.map f [_start.._end-1])
                |> Seq.concat)
        |> Map.values
        |> Seq.concat
    
    table.spawn
    |> Map.map (fun name (_start, _end) ->
        Dict ["start", Int _start; "end", Int _end; "pcs", Lst (liquidPcs table.agents.[name].init)])
    |> fun x -> ["initpcs", (Map.values >> Lst) x; "initenv", Lst env; "initvars", Lst vars; "tstamps", Lst tstamps]
    |> fun x -> (render x) init

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
                |> Set.map (fun g -> procExpr.BExprTranslator g true)
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