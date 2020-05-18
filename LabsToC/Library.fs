module LabsToC.LabsToC

open Frontend
open Frontend.LTS
open LabsCore.BExpr
open LabsCore.Grammar
open LabsCore.Tokens
open FSharpPlus

open LabsToC
open Outcome
open Common
open Liquid

/// Supported target languages.
type EncodeTo = | C | Lnt

let private encodeHeader trKit baseDict noBitvectors bound (table:SymbolTable) =
    let stigmergyVarsFromTo groupBy : Map<'a, (int*int)> =
        table.variables
        |> Map.filter (fun _ -> isLstigVar)
        |> Map.values
        |> Seq.groupBy groupBy
        |> Seq.map (fun (n, vars) ->
            let extrema = Seq.map (table.m.RangeOf) vars
            n, ((fst << Seq.minBy fst) extrema, (snd << Seq.maxBy snd) extrema))
        |> Map.ofSeq
        
    let tupleStart, tupleEnd, maxTuple = //TODO maybe move to frontend
        let vars = stigmergyVarsFromTo (fun v -> v.Location) |> Map.values |> Seq.sortBy fst
        let repeat fstOrSnd =
            Seq.concat << Seq.map (fun pair -> Seq.replicate (snd pair - fst pair + 1) (fstOrSnd pair))
        if Seq.isEmpty vars then seq [0], seq [0], 1 else
            repeat fst vars, repeat snd vars, Seq.map (fun (a, b) -> b - a + 1) vars |> Seq.max

    let getTypedef num nobv = 
        let getStandardTypes = 
            function
            | a, b when a >= 0     && b < 256      -> "unsigned char"
            | a, b when a >= -128   && b < 128      -> "char"
            | a, b when a >= 0     && b < 65536    -> "unsigned short"
            | a, b when a >= -32768 && b < 32768    -> "short"
            | a, _ when a >= 0                     -> "unsigned int"
            | _ -> "int "
        let bitwidth num = 
            System.Math.Log(float num, 2.) |> int |> (+) 1
        if nobv
        then getStandardTypes (0, num)
        else sprintf "unsigned __CPROVER_bitvector[%i]" (bitwidth num)
    
    let maxpc =
        Map.mapValues (fun (x:AgentTable) -> Map.keys x.initCond) table.agents
        |> Map.values |> Seq.concat |> Seq.max
    
    let maxcomponents = table.spawn |> Map.values |> Seq.map snd |> Seq.max
    let maxkeyE = max table.m.nextE 1
    let maxkeyI = max table.m.nextI 1
    let maxkeyL = max table.m.nextL 1
      
    let typedefs =
        [
            "TYPEOFVALUES", "short"
            "TYPEOFPC", "unsigned char"
            "TYPEOFTIME", "unsigned char" 
            "TYPEOFAGENTID", getTypedef maxcomponents noBitvectors
            "TYPEOFKEYEID", getTypedef maxkeyE noBitvectors
            "TYPEOFKEYIID", getTypedef maxkeyI noBitvectors
            "TYPEOFKEYLID", getTypedef maxkeyL noBitvectors
        ]
    
    let links =
        let fromTo = stigmergyVarsFromTo (fun v -> match v.Location with L (n, _) -> n | _ -> "")
        table.stigmergies
        |> Map.map (fun name link ->
            Dict [
                "start", fst fromTo.[name] |> Int
                "end", snd fromTo.[name] |> Int
                "link", trKit.LinkTr link |> Str
            ] 
        )
        |> Map.values
    
    let values =
        [
            "MAXCOMPONENTS", maxcomponents            
            "MAXPC", maxpc + 1
            "MAXTUPLE", maxTuple
        ]
        |> fun x -> x, List.map (fun (name, value) -> sprintf "typeof%s" name, getTypedef value true |> Str) x
        |> fun (x, y) -> List.append (List.map (fun (name, value) -> name, Int value) x) y
        
    [
        "typeofBOUND", getTypedef bound true |> Str
        "MAXKEYE", Int maxkeyE
        "MAXKEYI", Int maxkeyI
        "MAXKEYL", Int maxkeyL
        "typedefs", makeDict Str Str typedefs
        "links", Lst links
        "tupleStart", tupleStart |> Seq.map (Str << string) |> Lst
        "tupleEnd", tupleEnd |> Seq.map (Str << string) |> Lst
    ]
    |> List.append values
    |> List.append baseDict
    |> render (Liquid.parse (trKit.TemplateInfo.Get "header"))

let private encodeInit trKit (table:SymbolTable) =
    let env =
        table.variables
        |> Map.filter (fun _ -> isEnvVar)
        |> Map.values
        |> Seq.sortBy table.m.IndexOf
        |> Seq.collect (fun v ->
                let info = table.m.[v.Name]
                trKit.InitTr (v, snd table.m.[v.Name]) -1
                |> List.mapi (fun i x -> Dict ["type", Str "E"; "index", Int ((snd info) + i); "bexpr", Str x])
            )

    let agents =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
            table.agents.[name].variables
            |> List.append (table.agents.[name].lstigVariables table |> List.ofSeq)
            |> List.map (fun v tid ->
                let loc = match v.Location with I -> "I" | L _ -> "L" | E -> "E"
                trKit.InitTr (v, snd table.m.[v.Name]) tid
                |> List.map (fun x -> Dict ["loc", Str loc; "index", Int (snd table.m.[v.Name]); "bexpr", Str x])
                )
            |> List.collect (fun f -> List.map f [_start.._end-1])
            |> List.concat |> List.distinct
            |> fun l -> Dict ["start", Int _start; "end", Int _end; "initvars", Lst l; "pcs", liquidPcs table.agents.[name].initCond]
            )
        |> Map.values
        
    let tstamps =
        table.spawn
        |> Map.map (fun name (_start, _end) ->
                table.agents.[name].lstigVariables table
                |> Seq.map (fun v tid -> Dict ["tid", Int tid; "index", Int (snd table.m.[v.Name])])
                |> Seq.collect (fun f -> List.map f [_start.._end-1]))
        |> Map.values
        |> Seq.concat
    
    [
        "initenv", Lst env
        "agents", Lst agents
        "tstamps", Lst tstamps
        "hasStigmergy", Bool (table.m.nextL > 0)
        "hasEnvironment", Bool (table.m.nextE > 0)
    ]
    |> render (Liquid.parse (trKit.TemplateInfo.Get "init"))

let private funcName t =
    Map.map (sprintf "_%i_%i") t.entry |> Map.values
    |> String.concat ""
    |> (+) (if t.last then "_last" else "")

let private guards table t =
    table.guards.TryFind t.action |> Option.defaultValue Set.empty

let private encodeAgent trKit goto sync table (a:AgentTable) =
    let encodeTransition (t:Transition) =
        let guards = guards table t
        let assignments = t.action.Def |> (function Act a -> Some a | _ -> None)
        
        /// Set of keys that the agent will have to confirm
        let qrykeys =
            let getLstigVarsBExpr =
                let compareFn _ e1 e2 = Set.union (getLstigVars e1) (getLstigVars e2)
                cata (fun _ -> Set.empty) id compareFn (fun _ -> Set.unionMany)
            assignments
            |>> (fun a -> List.map (getLstigVars << snd) a.Updates)
            |>> Set.unionMany
            |> Option.orElse (Some Set.empty)
            |>> Set.union (guards |> Set.map getLstigVarsBExpr |> Set.unionMany)
            |>> Seq.map (Int << snd)
            |> Option.defaultValue (Seq.empty)
            |> Lst
        
        let liquidAssignment (k:Ref<Var<int>*int, unit>, expr) =
            let size = match (fst k.Var).Vartype with Array s -> s | _ -> 0
            Dict [
                "key",  Int (snd k.Var)
                "offset",
                    k.Offset |>> (trKit.AgentExprTr >> Str) |> Option.defaultValue (Int 0)
                "size", Int size
                "expr", trKit.AgentExprTr expr |> Str
            ]
        
        [
            "hasStigmergy", Bool (table.m.nextL > 0)
            "hasEnvironment", Bool (table.m.nextE > 0)
            "label", funcName t |> Str
            "last", t.last |> Bool
            "siblings", t.siblings |> Seq.map Int |> Lst
            "entrycond", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "exitcond", liquidPcs (t.exit)
            "guards", guards |> Seq.map (Str << (trKit.AgentGuardTr)) |> Lst
            "labs",
                // TODO do sth smart here
                string t.action.Def
                |> (+) (if guards.IsEmpty then "" else ((guards |> Set.map string |> String.concat " and ") + tGUARD)) 
                |> Str
            "loc",
                assignments
                |>> fun a -> a.ActionType
                |>> function | I -> "attr" | L _ -> "lstig" | E -> "env"
                |> Option.defaultValue ""
                |> Str
            "qrykeys", qrykeys
            "sync", sync |> Bool
            "assignments", assignments
                |>> fun a -> a.Updates
                |>> Seq.map liquidAssignment
                |> Option.defaultValue Seq.empty
                |> Lst         
        ]
        |> render goto
    
    Set.map (encodeTransition) a.lts
    |> Seq.reduce (<??>)

let private encodeMain trKit baseDict fair (table:SymbolTable) =
    let scheduleTransition t =
        Dict [
            "name", funcName t |> Str
            "siblings", seq t.siblings |> Seq.map Int |> Lst
            "entry", liquidPcs (t.entry |> Map.mapValues Set.singleton)
            "guards", guards table t |> Seq.map (Str << trKit.MainGuardTr) |> Lst
        ]
    let alwaysP, finallyP =
        let toLiquid props = makeDict Str Str (Seq.map (fun (n:Node<_>) -> n.Name, trKit.PropTr table n) props)
        let m1, m2 = Map.partition (fun _ n -> n.Def.Modality = Always) table.properties
        toLiquid <| Map.values m1, toLiquid <| Map.values m2
    
    [
        "firstagent", if table.spawn.Count = 1 then Int 0 else Int -1
        "fair", Bool fair
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
    |> List.append baseDict
    |> render (Liquid.parse (trKit.TemplateInfo.Get "main"))

let encode encodeTo bound (fair, nobitvector, sim, sync) table =
    let trKit = translateKit <| match encodeTo with | C -> C.wrapper | Lnt -> Lnt.wrapper
    let goto = Liquid.parse (trKit.TemplateInfo.Get "goto")
    
    let baseDict = [
        "bound", Int bound
        "hasStigmergy", Bool (table.m.nextL > 0)
        "hasEnvironment", Bool (table.m.nextE > 0)
        "simulation", Bool sim
    ]
    
    zero table
    <?> (encodeHeader trKit baseDict nobitvector bound)
    <?> (encodeInit trKit)
    <?> (fun x -> 
            (Map.values x.agents)
            |> Seq.map (encodeAgent trKit goto sync x)
            |> Seq.reduce (<??>))
    <?> (encodeMain trKit baseDict fair)
    <~~> zero () 