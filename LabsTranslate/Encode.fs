module internal Encode
open Types
open Base
open Templates
open Expressions
open Properties
open Liquid

type NodeType = Basic of Action<Var> | Skip | Goto | Stop

/// A Node is composed by a label, an entry/exit point,
/// and an action. Additional entry conditions are specified in the
/// "parent" set. Nodes can be augmented by guards.
type Node = {
    nodeType:NodeType
    label:string
    guards:Set<BExpr<Var, unit>>
    parent:Set<pcCondition>
    entry:pcCondition
    exit:pcCondition
} with 
    member this.lbl = 
        match this.nodeType with
        | Basic _
        | Skip _
        | Goto _ -> sprintf "%s_%i" this.label this.entry.value
        | Stop _ -> sprintf "Stop_%i" this.entry.value
    member this.entrypoints = 
        let x = Set.filter (fun p -> p.pc = this.entry.pc) this.parent
        if x.IsEmpty then (Set.add this.entry this.parent) else this.parent

let baseVisit (procs:Map<string, Process<Var>>) counter rootName =
    let pccount = makeCounter -1
    let rec visit name rootEntry cnt guards parent entry exit p lbl =
        let vs = visit name rootEntry cnt guards

        let node = {guards=guards; parent=parent; entry=entry; nodeType=Skip; exit=exit; label=lbl}

        match p with
        | Base a -> 
            Set.singleton {node with nodeType=Basic a}
        | Name s when s = name ->
            Set.singleton {node with nodeType=Goto; exit=rootEntry}
        | Name s -> visit s entry cnt guards parent entry exit procs.[s] (lbl)
        | Await(b, p) -> 
            visit name rootEntry cnt (guards.Add b) parent entry exit p lbl
        | Choice(p, q) -> 
            let pnodes = vs parent entry exit p (lbl+"_L")
            let qnodes = vs parent entry exit q (lbl+"_R")
            Set.union pnodes qnodes
        | Seq(p, q) ->
            let k = {pc=entry.pc;value=cnt()}
            let pnodes = vs parent entry k p lbl
            let qnodes =
                visit name rootEntry cnt Set.empty Set.empty k exit q lbl
            Set.union pnodes qnodes
        | Par(p, q) ->
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter -1, makeCounter -1
            let newPar = parent.Add entry
            let pnodes = visit name rootEntry lCount guards newPar {pc=leftPc;value=lCount()} {pc=leftPc;value=lCount()} p (lbl + "_L")
            let qnodes = visit name rootEntry rCount guards newPar {pc=rightPc;value=rCount()} {pc=rightPc;value=rCount()} q (lbl + "_R")
            Set.union pnodes qnodes
        | Process.Skip -> 
            Set.singleton {node with nodeType=Skip}
        | Nil -> 
            Set.singleton {node with nodeType=Stop}

    let pc = pccount()
    let entry = {pc=pc;value=counter()}
    (visit 
        rootName entry counter Set.empty Set.empty entry 
        {pc=pc;value=counter()} (procs.[rootName] ^. Nil) rootName), entry.value

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
            if node.parent.IsEmpty then node.entry.pc
            else
                (node.parent |> Set.map (fun x -> x.pc) |> Set.maxElement)
                |> max node.entry.pc
        trees
        |> Map.values
        |> Seq.map fst
        |> Set.unionMany
        |> Set.map (getPc)
        |> Set.maxElement
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
        let makeLink (s:Link.Stigmergy<Var>) = 
            let names =
                s.vars
                |> List.reduce Map.mergeIfDisjoint
                |> Map.keys
                |> Set.map (fun v -> v.name) 
            let m = mapping |> Map.filter (fun k _ -> names.Contains k)
            if m.IsEmpty then None else
            Dict [
                "start", Int (findMinIndex m)
                "end", Int (findMaxIndex m)
                "link", Str (translateLink mapping (sprintf "Stigmergy %s" s.name) s.link)
            ] |> Some

        sys.stigmergies
        |> Map.values
        |> Seq.choose makeLink

    [
        "defines", Lst defines
        "links", Lst links
    ]
    |> renderFile "templates/header.c"

let translateCanProceed (trees, mapping:KeyMapping) =
    let translatePc p = sprintf "(pc[tid][%i] == %i)" p.pc p.value

    trees
    |> Map.values
    |> Seq.collect fst
    |> Seq.map (fun n -> 
        n.guards 
        |> Set.map (translateGuard mapping n.label)
        |> Set.union (n.parent |> Set.map translatePc)
        |> Set.add (translatePc n.entry)
        |> Seq.map Str
        |> Lst
    )
    |> Seq.distinct
    |> fun x -> seq ["guards", Lst x]
    |> renderFile "templates/canProceed.c"

let translateAll (trees:Map<'b, (Set<Node> * 'c)>, mapping:KeyMapping) =
    let doOffset = function
        | Some e -> translateExpr mapping "" e |> Str
        | None -> Int 0
        
    let liquid a =
        let template, (k:Ref<Var, unit>), e =
            match a with
            | AttrUpdate(k, e) -> "attr", k, e
            | LStigUpdate(k, e) -> "lstig", k, e
            | EnvWrite(k, e) -> "env", k, e

        let _, index = mapping.[k.var.name]
        let size = match k.var.vartype with Array s -> s | _ -> 0

        [
            "labs", Str (string a)
            "type", Str template
            "key",  Int index
            "offset", doOffset k.offset
            "size", Int size
            "expr", translateExpr mapping (string a) e |> Str
            "qrykeys",
                getLstigVars e 
                |> Seq.map (fun v -> snd mapping.[v.name] |> Int)
                |> Lst
        ]

    let newTranslate (n:Node) = 

        let entries =
            n.entrypoints
            |> Seq.map (fun x -> Dict ["pc", Int x.pc; "value", Int x.value]) 
            
        let template, list = 
            match n.nodeType with
            | Basic a -> transition, liquid a
            | Skip -> goto, ["resetpcs", Bool false]
            | Goto -> goto, ["resetpcs", Bool true]
            | Stop -> stop, List.empty

        [
            "exitpc", Int n.exit.pc
            "exitvalue", Int n.exit.value;
            "label", Str n.lbl
            "entrypoints", Lst entries
            "guards",
                n.guards
                |> Set.map (translateGuard mapping n.label)
                |> Seq.map Str
                |> Lst
        ]
        |> Seq.append list
        |> strRender
        |> fun x -> Result.bind x template

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
        |> Seq.map fst
        |> Seq.concat
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
