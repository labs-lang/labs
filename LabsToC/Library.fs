module LabsToC.LabsToC

open Frontend
open Frontend.LTS
open Frontend.Liquid
open Types
open Tokens
open LabsCore
open FSharpPlus

open LabsToC
open Outcome
open Common 

let private encodeHeader trKit bound isSimulation noBitvectors (table:SymbolTable) =
    let stigmergyVarsFromTo groupBy : Map<'a, (int*int)> =
        table.variables
        |> Map.filter (fun _ -> isLstigVar)
        |> Map.values
        |> Seq.groupBy groupBy
        |> Seq.map (fun (n, s) -> n, Seq.map (table.m.RangeOf) s)
        |> Seq.map (fun (n, s) -> n, ((fst << Seq.minBy fst) s, (snd << Seq.maxBy snd) s))
        |> Map.ofSeq
        
    let tupleStart, tupleEnd, maxTuple = //TODO maybe move to frontend
        let vars = stigmergyVarsFromTo (fun v -> v.location) |> Map.values |> Seq.sortBy fst
        let repeat fstOrSnd =
            Seq.concat << Seq.map (fun pair -> Seq.replicate (snd pair - fst pair + 1) (fstOrSnd pair))
        if Seq.isEmpty vars then seq [0], seq [0], 1 else
            repeat fst vars, repeat snd vars, Seq.map (fun (a, b) -> b - a + 1) vars |> Seq.max

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
            "DISABLELSTIG", (if table.m.nextL = 0 then 1 else 0)
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
                "link", trKit.linkTr link |> Str
            ] 
        )
        |> Map.values
    
    [
        "MAXPC", maxpc + 1 |> Int
        "MAXKEYI", defines.["MAXKEYI"] |> Int
        "MAXKEYL", defines.["MAXKEYL"] |> Int
        "MAXKEYE", defines.["MAXKEYE"] |> Int
        "MAXCOMPONENTS", table.spawn |> Map.values |> Seq.map snd |> Seq.max |> Int
        "defines", defines |> Map.toSeq |> makeDict Str Int
        "typedefs", makeDict Str Str typedefs
        "links", Lst links
        "tupleStart", tupleStart |> Seq.map (Str << string) |> Lst
        "tupleEnd", tupleEnd |> Seq.map (Str << string) |> Lst
    ]
    |> render (Liquid.parse (trKit.templateInfo.Get "header"))

let private encodeInit trKit (table:SymbolTable) =
    let env =
        table.variables
        |> Map.filter (fun _ -> isEnvVar)
        |> Map.values
        |> Seq.sortBy table.m.IndexOf
        |> Seq.map (fun v ->
                let info = table.m.[v.name]
                trKit.initTr (v, snd table.m.[v.name]) -1
                |> List.mapi (fun i x -> Dict ["type", Str "E"; "index", Int ((snd info) + i); "bexpr", Str x])
            )
        |> Seq.concat

    let agents =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
            table.agents.[name].variables
            |> List.append (table.agents.[name].lstigVariables table |> List.ofSeq)
            |> List.map (fun v tid ->
                let loc = match v.location with I -> "I" | L _ -> "L" | E -> "E"
                trKit.initTr (v, snd table.m.[v.name]) tid
                |> List.map (fun x -> Dict ["loc", Str loc; "index", Int (snd table.m.[v.name]); "bexpr", Str x])
                )
            |> List.map (fun f -> List.map f [_start.._end-1])
            |> List.concat |> List.concat |> List.distinct
            |> fun l -> Dict ["start", Int _start; "end", Int _end; "initvars", Lst l; "pcs", liquidPcs table.agents.[name].initCond]
            )
        |> Map.values
        
    let tstamps =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
                table.agents.[name].lstigVariables table
                |> Seq.map (fun v tid -> Dict ["tid", Int tid; "index", Int (snd table.m.[v.name])])
                |> Seq.map (fun f -> List.map f [_start.._end-1])
                |> Seq.concat)
        |> Map.values
        |> Seq.concat
    
    ["initenv", Lst env; "agents", Lst agents; "tstamps", Lst tstamps]
    |> render (Liquid.parse (trKit.templateInfo.Get "init"))

let private funcName t =
    Map.map (sprintf "_%i_%i") t.entry |> Map.values
    |> String.concat ""
    |> (+) (if t.last then "_last" else "")

let private guards table t =
    table.guards.TryFind t.action |> Option.defaultValue Set.empty

let private encodeAgent trKit goto sync table (a:AgentTable) =
    let encodeTransition (t:Transition) =
        let guards = guards table t
        let assignments = t.action.def |> (function Act a -> Some a | _ -> None)
        
        /// Set of keys that the agent will have to confirm
        let qrykeys =
            let getLstigVarsBExpr =
                let compare_ _ e1 e2 = Set.union (getLstigVars e1) (getLstigVars e2)
                BExpr.cata (fun _ -> Set.empty) id compare_ (fun _ -> Set.unionMany)
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
                    k.offset |>> (trKit.agentExprTr >> Str) |> Option.defaultValue (Int 0)
                "size", Int size
                "expr", trKit.agentExprTr expr |> Str
            ]
        
        [
            "label", funcName t |> Str
            "last", t.last |> Bool
            "siblings", t.siblings |> Seq.map Int |> Lst
            "entrycond", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "exitcond", liquidPcs (t.exit)
            "guards", guards |> Seq.map (Str << (trKit.agentGuardTr)) |> Lst
            "labs",
                string t.action.def
                |> (+) (if guards.IsEmpty then "" else ((guards |> Set.map string |> String.concat " and ") + tGUARD)) 
                |> Str
            "loc",
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

let private encodeMain trKit isSimulation fair (table:SymbolTable) =
    let scheduleTransition t =
        Dict [
            "name", funcName t |> Str
            "siblings", seq t.siblings |> Seq.map Int |> Lst
            "entry", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "guards", guards table t |> Seq.map (Str << trKit.mainGuardTr) |> Lst
        ]
    let alwaysP, finallyP =
        let toLiquid props = makeDict Str Str (Seq.map (fun (n:Node<_>) -> n.name, trKit.propTr table n) props)
        let m1, m2 = Map.partition (fun _ n -> n.def.modality = Always) table.properties
        toLiquid <| Map.values m1, toLiquid <| Map.values m2
    
    [
        "firstagent", if table.spawn.Count = 1 then Int 0 else Int -1
        "fair", Bool fair
        "simulation", Bool isSimulation
        "schedule",
            table.agents
            |> Map.mapValues (fun a -> Seq.map scheduleTransition a.lts)
            |> Map.values
            |> Seq.concat
            |> Lst
        "alwaysasserts", alwaysP
        "finallyasserts", finallyP
        "agentscount", table.spawn |> Map.values |> Seq.map snd |> Seq.max |> Int
    ]
    |> render (Liquid.parse (trKit.templateInfo.Get "main"))

let encode encodeTo bound (fair, nobitvector, sim, sync) table =
    let trKit = translateKit <| match encodeTo with | C -> C.wrapper | Lnt -> Lnt.wrapper
    let goto = Liquid.parse (trKit.templateInfo.Get "goto")
    
    zero table
    <?> (encodeHeader trKit bound sim nobitvector)
    <?> (encodeInit trKit)
    <?> (fun x -> 
            (Map.values x.agents)
            |> Seq.map (encodeAgent trKit goto sync x)
            |> Seq.reduce (<??>))
    <?> (encodeMain trKit sim fair)
    <~~> zero () 