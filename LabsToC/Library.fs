module LabsToC.LabsToC

open Frontend
open Frontend.LTS
open Frontend.Liquid
open Expressions
open Types
open Tokens
open LabsCore
open FSharpPlus

open Outcome

let private goto = Liquid.parse "templates/goto.c"
let private init = Liquid.parse "templates/init.c"
let private header = Liquid.parse "templates/header.c"
let private main = Liquid.parse "templates/main.c"
let private UNDEF = -128 (*sentinel value for undef*)

let encodeHeader bound isSimulation noBitvectors (table:SymbolTable) =
    let stigmergyVarsFromTo groupBy : Map<'a, (int*int)> =
        table.variables
        |> Map.filter (fun _ -> isLstigVar)
        |> Map.values
        |> Seq.groupBy groupBy
        |> Seq.map (fun (n, s) -> n, Seq.map (table.m.RangeOf) s)
        |> Seq.map (fun (n, s) -> n, ((fst << Seq.minBy fst) s, (snd << Seq.maxBy snd) s))
        |> Map.ofSeq
        
    let tupleStart, tupleEnd, maxTuple = //TODO maybe move to frontend
        let vars = stigmergyVarsFromTo (fun v -> v.location) |> Map.values
        if Seq.isEmpty vars then seq [0], seq [0], 1 else
            vars
            |> fun x -> Seq.sort <| Seq.map fst x, Seq.sort <| Seq.map snd x
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
        Map.mapValues (fun (x:AgentTable) -> Map.keys x.initCond) table.agents
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
    
    let links =
        let fromTo = stigmergyVarsFromTo (fun v -> match v.location with L (n, _) -> n | _ -> "")
        table.stigmergies
        |> Map.map (fun name link ->
            Dict [
                "start", fst fromTo.[name] |> Int
                "end", snd fromTo.[name] |> Int
                "link", link |> linkExpr.BExprTranslator true |> Str
            ] 
        )
        |> Map.values
    
    [
        "defines", defines |> Map.toSeq |> makeDict Str Int
        "typedefs", makeDict Str Str typedefs
        "links", Lst links
        "tupleStart", tupleStart |> Seq.map (Str << string) |> Lst
        "tupleEnd", tupleEnd |> Seq.map (Str << string) |> Lst
    ]
    |> render header

let encodeInit (table:SymbolTable) =
    let env =
        table.variables
        |> Map.filter (fun _ -> isEnvVar)
        |> Map.values
        |> Seq.sortBy table.m.IndexOf
        |> Seq.map (fun v ->
                Frontend.Frontend.initBExprs UNDEF Expr.evalCexprNoId table.m.[v.name]
                |> List.map ((initExpr "").BExprTranslator false >> Str)
            )
        |> Seq.concat
    
    let vars =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
            table.agents.[name].variables
            |> List.append (table.agents.[name].lstigVariables table |> List.ofSeq)
            |> List.map (fun v tid ->
                Frontend.initBExprs UNDEF (Expr.evalConstExpr (fun _ -> tid)) (v, snd table.m.[v.name])
                |> List.map ((initExpr (string tid)).BExprTranslator false))
                |> List.map (fun x i -> List.map Str (x i))
            |> List.map (fun f -> List.map f [_start.._end-1])
            |> List.concat |> List.concat)
        |> Map.values
        |> List.concat
        
    let tstamps =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
                table.agents.[name].lstigVariables table
                |> Seq.map (fun v tid -> Dict ["tid", Int tid; "index", Int (snd table.m.[v.name])])
                |> Seq.map (fun f -> List.map f [_start.._end-1])
                |> Seq.concat)
        |> Map.values
        |> Seq.concat
    
    table.spawn
    |> Map.map (fun name (_start, _end) ->
        Dict ["start", Int _start; "end", Int _end; "pcs", liquidPcs table.agents.[name].init])
    |> fun x -> ["initpcs", (Map.values >> Lst) x; "initenv", Lst env; "initvars", Lst vars; "tstamps", Lst tstamps]
    |> render init

let private funcName t =
    Map.map (sprintf "_%i_%i") t.entry |> Map.values
    |> String.concat ""
    |> (+) (if t.last then "_last" else "")

let private guards table t =
    table.guards.TryFind t.action |> Option.defaultValue Set.empty

let encodeAgent sync table (a:AgentTable) =
    let encodeTransition (t:Transition) =
        let guards = guards table t
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
            "label", funcName t |> Str
            "last", t.last |> Bool
            "siblings", t.siblings |> Seq.map Int |> Lst
            "entrycond", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "exitcond", liquidPcs (t.exit)
            "guards", guards |> Seq.map (Str << (procExpr.BExprTranslator true)) |> Lst
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
        |> render goto
    
    Set.map (encodeTransition) a.lts
    |> Seq.reduce (<??>)

let encodeMain fair (table:SymbolTable) =
    let scheduleTransition t =
        Dict [
            "name", funcName t |> Str
            "siblings", seq t.siblings |> Seq.map Int |> Lst
            "entry", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "guards", guards table t |> Seq.map (Str << ((tidProcExpr "firstAgent").BExprTranslator true)) |> Lst
        ]
    let alwaysP, finallyP =
        let toLiquid props = makeDict Str Str (Seq.map (fun (n:Node<_>) -> n.name, translateProp table n) props)
        let m1, m2 = Map.partition (fun _ n -> n.def.modality = Always) table.properties
        toLiquid <| Map.values m1, toLiquid <| Map.values m2
    
    [
        "firstagent", if table.spawn.Count = 1 then Int 0 else Int -1
        "fair", Bool fair;
        "schedule",
            table.agents
            |> Map.mapValues (fun a -> Seq.map scheduleTransition a.lts)
            |> Map.values
            |> Seq.concat
            |> Lst
        "alwaysasserts", alwaysP
        "finallyasserts", finallyP
    ]
    |> render main

let encode bound (fair, nobitvector, sim, sync) table =
    zero table
    <?> (encodeHeader bound sim nobitvector)
    <?> encodeInit
    <?> (fun x -> 
            (Map.values x.agents)
            |> Seq.map (encodeAgent sync x)
            |> Seq.reduce (<??>))
    <?> (encodeMain fair)
    <~~> zero () 