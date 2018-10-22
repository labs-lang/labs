module internal Encode
open Types
open Link
open Base
open Templates
open Expressions
open Properties
open Liquid

type NodeType = Basic of Action<Var * int> | Goto | Stop

type Node = {
    nodeType:NodeType
    label:string
    guards:Set<BExpr<Var * int, unit>>
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

let baseVisit (procs:Map<string, Process<Var*int>>) counter rootName =

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

let encode (sys:SystemDef<Var*int>) = 
    if sys.SpawnedComps.IsEmpty then failwith "No components have been spawned!"
    let counter = makeCounter -1

    sys.SpawnedComps
    |> Map.mapValues (fun def -> (def, Map.merge sys.processes def.processes))
    |> Map.mapValues (fun (_, procs) -> baseVisit procs counter "Behavior")
    |> fun x -> Ok (sys, x)

let translateHeader isSimulation ((sys, trees, mapping:KeyMapping, maxI, maxL, maxE), bound) =

    let tupleStart, tupleEnd =
        /// Finds the min and max indexes of the given tuple.
        let extrema (tup:Set<Var>) =
            let indexes = 
                tup
                |> Set.map (fun v -> mapping.[v.name])
            let endIndexes = 
                tup
                |> Set.map (fun v ->
                    match v.vartype with
                    | Scalar -> 0
                    | Array n -> n - 1 
                    |> (+) mapping.[v.name])

            (Seq.min indexes, Seq.max endIndexes)
        let makeTuple (tup: Set<Var>) =
            let min, max = extrema tup
            List.replicate (max-min+1) (min, max)
        
        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.collect (List.map (makeTuple))
        |> List.concat
        |> List.unzip

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

    let defines = 
        [
            "BOUND", bound; 
            "MAXCOMPONENTS", maxcomps;
            "MAXPC", maxPc;
            "MAXKEYI", maxI
            "MAXKEYL", maxL
            "MAXKEYE", maxE
        ]
        |> (if isSimulation then fun l -> ("SIMULATION", 1)::l else id)
        |> List.map (fun (a,b) -> Dict ["name", Str a; "value", Int b])

    let links =
        let makeLink (s:Stigmergy<Var*int>) = 
            let names =
                s.vars
                |> Set.unionMany
                |> Set.map (fun v -> v.name)
            let m = mapping |> Map.filter (fun k _ -> names.Contains k)
            if m.IsEmpty then None else
            Dict [
                "start", Int (Map.values m |> Seq.min)
                "end", Int (Map.values m |> Seq.max)
                "link", s.link |> linkExpr.BExprTranslator (sprintf "Stigmergy %s" s.name) |> Str
            ] |> Some

        sys.stigmergies
        |> Map.values
        |> Seq.choose makeLink

    [
        "defines", Lst defines
        "links", Lst links
        "tupleStart", tupleStart |> Seq.map (Str << string) |> Lst
        "tupleEnd", tupleEnd |> Seq.map (Str << string) |> Lst
    ]
    |> renderFile "templates/header.c"

let translateGuard = procExpr.BExprTranslator

let translateCanProceed trees =
    let translatePc = sprintf "(pc[tid][%i] == %i)"
    trees
    |> Map.values
    |> Seq.collect fst
    |> Seq.map (fun n -> 
        n.guards 
        |> Seq.map (procExpr.BExprTranslator n.label)
        |> Seq.append (n.entry |> Map.map translatePc |> Map.values)
        |> Seq.map Str
        |> Lst
    )
    |> Seq.distinct
    |> fun x -> seq ["guards", Lst x]
    |> renderFile "templates/canProceed.c"

let translateAll (trees:Map<'b, (Set<Node> * 'c)>) =
    let doOffset = function
        | Some e -> procExpr.ExprTranslator "" e |> Str
        | None -> Int 0

    let liquid a guards =
        let template =
            match a.actionType with
            | I -> "attr"
            | L _ -> "lstig"
            | E -> "env"

        let qrykeys =
            a.updates
            |> List.map (getLstigVars << snd)
            |> Set.unionMany
            |> Seq.map (Int << snd)
            |> Lst

        let liquidAssignment (k:Ref<Var*int, unit>, expr) =
            let size = match (fst k.var).vartype with Array s -> s | _ -> 0
            seq [
                "key",  Int (snd k.var)
                "offset", doOffset k.offset
                "size", Int size
                "expr", procExpr.ExprTranslator (string a) expr |> Str
            ]
            |> Dict

        let g = 
            if Set.isEmpty guards then ""
            else 
                guards
                |> Set.map (sprintf "%O")
                |> String.concat " && "
                |> fun s -> s + " -> " 
        [
            "labs", Str (string a |> (+) g)
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
            | Basic a -> transition, liquid a n.guards
            | Goto -> goto, List.empty
            | Stop -> stop, List.empty

        [
            "label", Str n.lbl
            "entrypoints", liquidPcs n.entry |> Lst
            "exitpoints", liquidPcs n.exit |> Lst
            "guards",
                n.guards
                |> Set.map (procExpr.BExprTranslator n.label)
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

let translateMain fair (sys:SystemDef<Var*int>, trees:Map<string, Set<Node> * 'a>) =
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
        |> Map.mapValues (translateProp sys)
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
