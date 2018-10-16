module internal Encode
open Types
open Link
open Base
open Templates
open Expressions
open Properties
open Liquid

type NodeType = Basic of Action<Var> | Goto | Stop

type Node = {
    nodeType:NodeType
    label:string
    guards:Set<BExpr<Var, unit>>
    pc:int
    entry: Map<int, int>
    exit: Map<int,int>
} with 
    member this.lbl = 
        let pc = this.entry |> Map.keys |> Set.maxElement
        match this.nodeType with
        | Basic _
        | Goto _ -> sprintf "%s_%i" this.label this.entry.[pc]
        | Stop _ -> sprintf "Stop_%i" this.entry.[pc]

let baseVisit (procs:Map<string, Process<Var>>) counter rootName =

    let pccount = makeCounter -1
    let rec visit name rootEntry cnt pc guards entry exit proc lbl =
        let vs = visit name rootEntry cnt pc

        let node = {guards=guards; pc=pc; entry=entry; nodeType=Goto; exit=exit; label=lbl}

        match proc with
        | Base a -> 
            Set.singleton {node with nodeType=Basic a}
        | Name s when s = name ->
            Set.singleton {node with exit=rootEntry}
        | Name s -> visit s entry cnt pc guards entry exit procs.[s] (lbl)
        | Await(b, p) -> 
            visit name rootEntry cnt pc (guards.Add b) entry exit p lbl
        | Choice(p, q) -> 
            let pnodes = vs guards entry exit p (lbl+"_L")
            let qnodes = vs guards entry exit q (lbl+"_R")
            Set.union pnodes qnodes
        | Seq(p, q) ->
            let pexit = Map.empty.Add(pc, cnt())
            let pnodes = vs guards entry pexit p lbl
            let qnodes = vs Set.empty pexit exit q lbl
            Set.union pnodes qnodes
        | Par(p, q) ->
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter -1, makeCounter -1
            let pentry = entry.Add(leftPc, lCount())
            let pexit = exit.Add(leftPc, lCount()).Remove(pc)
            let qentry = entry.Add(rightPc, rCount())
            let qexit = exit.Add(rightPc, rCount()).Remove(pc)

            let pnodes = visit name rootEntry lCount leftPc guards pentry pexit p (lbl + "_L")
            let qnodes = visit name rootEntry rCount rightPc guards qentry qexit q (lbl + "_R")

            let joinEntry = Map.merge pexit qexit
            let joinExit = joinEntry |> Map.mapValues (fun _ -> 0) |> Map.merge exit
            Set.union pnodes qnodes
            |> Set.add {node with entry=joinEntry; exit=joinExit; nodeType=Goto}

        | Process.Skip -> 
            Set.singleton node
        | Nil -> 
            Set.singleton {node with nodeType=Stop}

    let pc = pccount()
    let enM = Map.empty.Add(pc, counter())
    let exM = Map.empty.Add(pc, counter())

    (visit 
        rootName enM counter pc Set.empty enM exM (procs.[rootName] ^. Nil) rootName), enM.[pc]

let encode (sys:SystemDef<Var>, mapping) = 
    if sys.SpawnedComps.IsEmpty then failwith "No components have been spawned!"
    let counter = makeCounter -1

    let trees = 
        sys.SpawnedComps
        |> Map.mapValues (fun def -> (def, Map.merge sys.processes def.processes))
        |> Map.mapValues (fun (_, procs) -> baseVisit procs counter "Behavior")

    Result.Ok(sys, trees, mapping)

    
let translateHeader isSimulation ((sys, trees, mapping:KeyMapping), bound) =
    // Find the number of PCs used in the program
    let maxPc =
        let getPc node = 
            node.entry |> Map.keys |> Set.maxElement

        trees
        |> Map.values
        |> Seq.collect fst
        |> Seq.map getPc
        |> Seq.max
        |> (+) 1

    let maxcomps = 
        Map.fold (fun state _ (_, cmax) -> max state cmax) 0 sys.spawn

    let ifaces = mapping |> Map.filter (fun _ (v, _) -> v.location = I)
    let env = mapping |> Map.filter (fun _ (v, _) -> v.location = E)
    let lstig =
        mapping
        |> Map.filter (fun _ (v, _) -> match v.location with L _ -> true | _ -> false)

    let defines = 
        [
            "BOUND", bound; 
            "MAXCOMPONENTS", maxcomps;
            "MAXPC", maxPc;
            "MAXKEYI", ((findMaxIndex ifaces) + 1)
            "MAXKEYL", ((findMaxIndex lstig) + 1)
            "MAXKEYE", ((findMaxIndex env) + 1)
        ]
        |> (if isSimulation then fun l -> ("SIMULATION", 1)::l else id)
        |> List.map (fun (a,b) -> Dict ["name", Str a; "value", Int b])

    let links =
        let makeLink (s:Stigmergy<Var>) = 
            let names =
                s.vars
                |> Set.unionMany
                |> Set.map (fun v -> v.name) 
            let m = mapping |> Map.filter (fun k _ -> names.Contains k)
            if m.IsEmpty then None else
            Dict [
                "start", Int (findMinIndex m)
                "end", Int (findMaxIndex m)
                "link", s.link |> (linkExpr mapping).BExprTranslator (sprintf "Stigmergy %s" s.name) |> Str
            ] |> Some

        sys.stigmergies
        |> Map.values
        |> Seq.choose makeLink

    [
        "defines", Lst defines
        "links", Lst links
    ]
    |> renderFile "templates/header.c"

let translateGuard mapping = (procExpr mapping).BExprTranslator

let translateCanProceed (trees, mapping:KeyMapping) =
    let translatePc = sprintf "(pc[tid][%i] == %i)"
    trees
    |> Map.values
    |> Seq.collect fst
    |> Seq.map (fun n -> 
        n.guards 
        |> Seq.map (translateGuard mapping n.label)
        |> Seq.append (n.entry |> Map.map translatePc |> Map.values)
        |> Seq.map Str
        |> Lst
    )
    |> Seq.distinct
    |> fun x -> seq ["guards", Lst x]
    |> renderFile "templates/canProceed.c"

let translateAll (trees:Map<'b, (Set<Node> * 'c)>, mapping:KeyMapping) =
    let translateExpr = (procExpr mapping).ExprTranslator
    let doOffset = function
        | Some e -> translateExpr "" e |> Str
        | None -> Int 0

    let liquid a =
        let template =
            match a.actionType with
            | I -> "attr"
            | L _ -> "lstig"
            | E -> "env"

        let qrykeys =
            a.updates
            |> List.map (getLstigVars << snd)
            |> Set.unionMany
            |> Seq.map (fun v -> snd mapping.[v.name] |> Int)
            |> Lst

        let liquidAssignment (k:Ref<Var, unit>, expr) =
            let _, index = mapping.[k.var.name]
            let size = match k.var.vartype with Array s -> s | _ -> 0
            seq [
                "key",  Int index
                "offset", doOffset k.offset
                "size", Int size
                "expr", translateExpr (string a) expr |> Str
            ]
            |> Dict

        [
            "labs", Str (string a)
            "type", Str template
            "qrykeys", qrykeys
            "assignments", a.updates |> Seq.map liquidAssignment |> Lst
        ]

    let newTranslate (n:Node) = 

        let liquidPcs map =
            map
            |> Map.map (fun p v -> Dict ["pc", Int p; "value", Int v]) 
            |> Map.values
            
        let template, list = 
            match n.nodeType with
            | Basic a -> transition, liquid a
            | Goto -> goto, List.empty
            | Stop -> stop, List.empty

        [
            "label", Str n.lbl
            "entrypoints", liquidPcs n.entry |> Lst
            "exitpoints", liquidPcs n.exit |> Lst
            "guards",
                n.guards
                |> Set.map (translateGuard mapping n.label)
                |> Seq.map Str
                |> Lst
        ]
        |> Seq.append list
        |> strRender
        |> fun x -> Result.bind x template |> Result.mapError (failwith)

    trees
    |> Map.values
    |> Seq.collect fst
    |> Set.ofSeq
    |> Seq.map newTranslate
    |> Seq.reduce (fun r1 r2 -> r1 >+> r2 |> Result.map (fun (s1:string, s2) -> s1 + s2) )

let translateMain fair (sys, trees:Map<string, Set<Node> * 'a>, mapping) =
    let nodes = 
        trees
        |> Map.values 
        |> Seq.collect fst
    let finallyP, alwaysP =
        sys.properties
        |> Map.partition (
            fun _ {modality=m} -> 
                match m with Finally -> true | Always -> false)
    let translateProperties props =
        props
        |> Map.mapValues (translateProp sys mapping)
        |> Map.values
        |> String.concat "\n"

    [
        "fair", Bool fair;
        "schedule", nodes |> Seq.map (fun n -> Str n.lbl) |> Lst
        "alwaysasserts",
            alwaysP
            |> translateProperties
            |> indent 4 |> Str
        "finallyasserts",  
            finallyP
            |> translateProperties
            |> indent 4 |> Str
    ]
    |> renderFile "templates/main.c"
