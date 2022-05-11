module LabsTranslate.Encode

open Frontend
open LabsCore.Grammar
open Frontend.STS
open LabsCore.ExprTypes
open LabsCore.BExpr
open LabsCore.Tokens
open FSharpPlus

open Outcome
open TranslationKit
open Liquid

/// Supported target languages.
type EncodeTo = | C | Lnt | Lnt_Monitor | Lnt_Parallel

let private encodeHeader trKit baseDict noBitvectors bound (table:SymbolTable) =
    let stigmergyVarsFromTo groupBy : Map<'a, int*int> =
        table.Variables
        |> Map.filter (fun _ -> isLstigVar)
        |> Map.values
        |> Seq.groupBy groupBy
        |> Seq.map (fun (n, vars) ->
            let extrema = Seq.map table.M.RangeOf vars
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
        else $"unsigned __CPROVER_bitvector[%i{bitwidth num}]"
    
    let maxpc =
        Map.mapValues (fun (x:AgentTable) -> Map.keys x.InitCond) table.Agents
        |> Map.values |> Seq.concat |> Seq.max
    
    let maxcomponents = table.Spawn |> Map.values |> Seq.map snd |> Seq.max
    let maxkeyE = max table.M.NextE 1
    let maxkeyI = max table.M.NextI 1
    let maxkeyL = max table.M.NextL 1
      
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
        table.Stigmergies
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
        |> fun x -> x, List.map (fun (name, value) -> $"typeof%s{name}", getTypedef value true |> Str) x
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
    |> render (parse (trKit.TemplateInfo.Get "header"))

let private encodeInit trKit baseDict (table:SymbolTable) =
    let env =
        table.Variables
        |> Map.filter (fun _ -> isEnvVar)
        |> Map.values
        |> Seq.sortBy table.M.IndexOf
        |> Seq.collect (fun v ->
                let info = table.M.[v.Name]
                trKit.InitTr (v, snd table.M.[v.Name]) -1
                |> List.mapi (fun i x -> Dict ["type", Str "E"; "index", Int ((snd info) + i); "bexpr", Str x])
            )

    let loc v = match v.Location with I -> "I" | L _ -> "L" | E -> "E" | Local -> "Local" | Pick _ -> "Pick"
    let agents =
        table.Spawn
        |> Map.map (fun name (_start, _end) ->
            table.Agents.[name].Attributes
            |> List.append (table.Agents.[name].LstigVariables table |> List.ofSeq)
            |> List.map (fun v tid ->
                trKit.InitTr (v, snd table.M.[v.Name]) tid
                |> List.map (fun x -> Dict ["loc", Str (loc v); "index", Int (snd table.M.[v.Name]); "bexpr", Str x])
                )
            |> List.collect (fun f -> List.map f [_start.._end-1])
            |> List.concat |> List.distinct
            |> fun l -> Dict ["start", Int _start; "end", Int _end; "initvars", Lst l; "pcs", liquidPcs table.Agents.[name].InitCond]
            )
        |> Map.values
        
    let tstamps =
        table.Spawn
        |> Map.map (fun name (_start, _end) ->
                table.Agents.[name].LstigVariables table
                |> Seq.map (fun v tid -> Dict ["tid", Int tid; "index", Int (snd table.M.[v.Name])])
                |> Seq.collect (fun f -> List.map f [_start.._end-1]))
        |> Map.values
        |> Seq.concat
    
    let assumes =
        makeDict Str Str (Seq.map (fun (n:Node<_>) -> n.Name, trKit.PropTr table n) (Map.values table.Assumes))
    
    [
        "assumes", assumes
        "initenv", Lst env
        "agents", Lst agents
        "tstamps", Lst tstamps  
    ]
    |> List.append baseDict
    |> render (parse (trKit.TemplateInfo.Get "init"))

let private funcName t =
    Map.map (sprintf "_%i_%i") t.Entry |> Map.values
    |> String.concat ""
    |> (+) (if t.Last then "_last" else "")

let private guards table t =
    table.Guards.TryFind t.Action |> Option.defaultValue Set.empty

let private encodeAgent trKit baseDict goto block sync table (a:AgentTable) =
    let encodeTransition (t:Transition) =
        let guards = guards table t
        let assignments = t.Action.Def |> (function Act a -> Some a | _ -> None)
        
        // LStig Variables that are being updated
        // and therefore should not be queried  
        let LStigVarsAssignedTo =
            assignments
            |>> (fun a -> List.map fst a.Updates)
            |>> (fun x -> List.map (fun r -> r.Var) x)
            |>> (List.filter (fst >> isLstigVar))
            |>> Set.ofList
            |> Option.defaultValue Set.empty
            
        
        /// Set of keys that the agent will have to confirm
        /// TODO maybe move to Frontend?
        let qrykeys =
            let getLstigVarsBExpr =
                let compareFn _ e1 e2 = Set.union (getLstigVars e1) (getLstigVars e2)
                cata (fun _ -> Set.empty) id compareFn (fun _ -> Set.unionMany)
            assignments
            |>> (fun a -> List.map (getLstigVars << snd) a.Updates)
            |>> Set.unionMany
            |> Option.orElse (Some Set.empty)
            |>> Set.union (guards |> Set.map getLstigVarsBExpr |> Set.unionMany)
            |>> fun s -> Set.difference s LStigVarsAssignedTo 
            |>> Seq.map (Int << snd)
            |> Option.defaultValue Seq.empty
            |> Lst
        
        let liquidAssignment (k:Ref<Var<int>*int, unit>, expr) =
            let v = fst k.Var
            let size = match v.Vartype with Array s -> s | _ -> 0
            let loc =
                v.Location
                |> function | I -> "attr" | L _ -> "lstig" | E -> "env" | Local -> "Local" | Pick _ -> "Pick" 

            Dict [
                "name", Str v.Name
                "loc", Str loc    
                "key",  Int (snd k.Var)
                "offset", k.Offset |>> (trKit.AgentExprTr >> Str) |> Option.defaultValue (Int 0)
                "size", Int size
                "expr", trKit.AgentExprTr expr |> Str
            ]
        
        let auxs =
            assignments
            |>> fun a -> a.Updates
            |>> Seq.map (snd >> trKit.CollectAuxVars)
            |>> Set.unionMany
            |> Option.defaultValue Set.empty
            |> Seq.map (fun (a, b, c) -> Lst [ Str a; Str b; Str c ])
            |> Lst
            
        [
            "aux", auxs
            "label", funcName t |> Str
            "last", t.Last |> Bool
            "siblings", t.Siblings |> Seq.map Int |> Lst
            "entrycond", liquidPcs (t.Entry |> Map.mapValues Set.singleton)
            "exitcond", liquidPcs t.Exit
            "guards", guards |> Seq.map (Str << trKit.AgentGuardTr) |> Lst
            "labs",
                // TODO do sth smart here
                string t.Action.Def
                |> (+) (if guards.IsEmpty then "" else ((guards |> Set.map string |> String.concat " and ") + tGUARD)) 
                |> Str
            "qrykeys", qrykeys
            "sync", sync |> Bool
            "assignments", assignments
                |>> fun a -> a.Updates
                |>> Seq.map liquidAssignment
                |> Option.defaultValue Seq.empty
                |> Lst         
        ]
        |> List.append baseDict
    
    let encoder (t:Transition) =
        match t.Action.Def with
        | Block stmts ->
            let encodes =
                List.map (fun a -> {t with Action={t.Action with Def=Act a}} |> encodeTransition |> Map.ofList) stmts
            let hd = encodes.Head
            let locals =
                let liquidVar v =
                    let pickFrom, pickTo =
                        match v.Location with
                        | Pick (_, Some typ, _) -> SymbolTable.findAgent table typ t.Action.Pos |> fun (a, b, _) -> a, b
                        | _ -> 0, table.Spawn |> Map.values |> Seq.map snd |> Seq.max
                    Dict [
                    "name", v.Name |> Str
                    "loc", string v.Location |> Str 
                    "size", Int <| match v.Vartype with Scalar -> 0 | Array s -> s
                    "pickFrom", Int pickFrom
                    "pickTo", Int pickTo
                    "where",
                        match v.Location with
                        | Pick (_, _, Some w) -> w |> table.TranslateBExpr |> trKit.LinkTr
                        | _ -> ""
                        |> Str
                ]
                stmts
                |> Seq.map (fun a -> List.map (fst >> fun r -> fst r.Var) a.Updates)
                |> Seq.concat
                |> Seq.filter (fun v -> match v.Location with Local | Pick _ -> true | _ -> false)
                |> Seq.distinctBy (fun v -> v.Name)
                |> Seq.map liquidVar
                |> Lst

            [
                "guards", guards table t |> Seq.map (Str << trKit.AgentGuardTr) |> Lst
                "locals", locals 
                "assignments", seq { for e in encodes -> e.["assignments"] } |> Lst
                "labs", seq { for e in encodes -> e.["labs"] } |> Lst
                "qrykeys",  seq { for e in encodes -> e.["qrykeys"] } |> Lst
            ]
            |> List.append baseDict
            |> Map.ofList
            |> fun d -> Map.union d hd
            |> Map.toList
            |> render block
        | _ -> encodeTransition t |> render goto
    
    a.Sts |> Set.map encoder |> Seq.reduce (<??>)

let private encodeMain trKit baseDict fair noprops prop (table:SymbolTable) =
    let scheduleTransition t =
        Dict [
            "name", funcName t |> Str
            "siblings", seq t.Siblings |> Seq.map Int |> Lst
            "entry", liquidPcs (t.Entry |> Map.mapValues Set.singleton)
            "guards", guards table t |> Seq.map (Str << trKit.MainGuardTr) |> Lst
        ]
    
    let toLiquid props = makeDict Str Str (Seq.map (fun (n:Node<_>) -> n.Name, trKit.PropTr table n) props)
    let filterPropsByModality modality =
        let maybeFilter m =
            match prop with
            | Some p ->
                let m' = Map.filter (fun k _ -> k = p) m
                if m'.IsEmpty then failwith $"Property {p} not found." else ()
                m'
            | None -> m
        table.Properties
        |> maybeFilter
        |> Map.filter (fun _ n -> n.Def.Modality = modality)
        |> Map.values
        
    let alwaysP = (if noprops then Seq.empty else filterPropsByModality Always) |> toLiquid
    let eventuallyP = (if noprops then Seq.empty else filterPropsByModality Eventually) |> toLiquid
    let finallyP = (if noprops then Seq.empty else filterPropsByModality Finally) |> toLiquid
        
    [
        "firstagent", if table.Spawn.Count = 1 then Int 0 else Int -1
        "fair", Bool fair
        "schedule",
            table.Agents
            |> Map.mapValues (fun a -> Seq.map scheduleTransition a.Sts)
            |> Map.values
            |> Seq.concat
            |> Lst
        "alwaysasserts", alwaysP
        "finallyasserts", finallyP
        "eventuallypredicates", eventuallyP
        "agentscount", table.Spawn |> Map.values |> Seq.map snd |> Seq.max |> Int
    ]
    |> List.append baseDict
    |> render (parse (trKit.TemplateInfo.Get "main"))

let encode encodeTo bound (fair, nobitvector, sim, sync, noprops) prop table =
    let trKit = makeTranslationKit <|
                match encodeTo with
                | C -> C.wrapper
                | Lnt -> Lnt.wrapper
                | Lnt_Monitor -> Lnt.wrapperMonitor
                | Lnt_Parallel -> Lnt.wrapperParallel
    let goto = parse (trKit.TemplateInfo.Get "goto")
    let block = parse (trKit.TemplateInfo.Get "block")
    
    let baseDict = [
        "bound", Int bound
        "hasStigmergy", Bool (table.M.NextL > 0)
        "hasEnvironment", Bool (table.M.NextE > 0)
        "MAXCOMPONENTS", table.Spawn |> Map.values |> Seq.map snd |> Seq.max |> Int
        "simulation", Bool sim
    ]
    
    zero table
    <?> (encodeHeader trKit baseDict nobitvector bound)
    <?> (encodeInit trKit baseDict)
    <?> (fun x -> 
            (Map.values x.Agents)
            |> Seq.map (encodeAgent trKit baseDict goto block sync x)
            |> Seq.reduce (<??>))
    <?> (encodeMain trKit baseDict fair noprops prop)
    <~~> zero () 