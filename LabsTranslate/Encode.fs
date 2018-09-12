module internal Encode
open Types
open Base
open Templates
open Expressions
open Properties
open Liquid

/// A Node is composed by a label, an entry/exit point,
/// and an action. Additional entry conditions are specified in the
/// "parent" set. Nodes can be augmented by guards.
type Node = 
| Basic of parent:Set<pcCondition> * entry:pcCondition * Action<Var> * exit:pcCondition * lbl:string
| Guarded of string * Node
| Goto of parent:Set<pcCondition> * entry:pcCondition * exit:pcCondition * lbl:string
| Stop of parent:Set<pcCondition> * entry:pcCondition
with 
    member this.lbl = 
        match this with
        | Basic(lbl=l;entry=e)
        | Goto(lbl=l;entry=e) -> sprintf "%s_%i" l e.value
        | Stop(entry=e) -> sprintf "Stop_%i" e.value
        | Guarded(_,n) -> n.lbl
    member this.entrypoints = 
        match this with
        | Guarded(_,n) -> n.entrypoints
        | Basic(parent=par;entry=e)
        | Goto(parent=par;entry=e)
        | Stop(parent=par;entry=e) ->
            let x = Set.filter (fun p -> p.pc = e.pc) par
            if x.IsEmpty then Set.add e par else par

let baseVisit (procs:Map<string, Process<Var>>) counter mapping rootName =
    let pccount = makeCounter -1
    let rec visit name rootEntry cnt parent entry exit p lbl =
        let vs = visit name rootEntry cnt
        let parentUnion = Set.union parent

        match p with
        | Base(a) -> 
            Set.singleton <| Basic(parent, entry, a, exit, lbl), Set.singleton exit
        | Name(s) when s = name ->
            Set.singleton <| Goto(parent, entry, rootEntry, lbl), Set.empty
        | Name(s) -> visit name rootEntry cnt parent entry exit procs.[s] (lbl)
        | Await(b, p) -> 
            let pnodes, pexits = (vs parent entry exit p lbl)
            pnodes
            |> Set.map (fun n -> Guarded((assume (translateGuard mapping b)), n))
            |> fun nodes -> nodes, pexits
        | Choice(p, q) -> 
            let pnodes, pexits = (vs parent entry exit p (lbl+"_L"))
            let qnodes, qexits = (vs parent entry exit q (lbl+"_R"))
            (Set.union pnodes qnodes), (Set.union pexits qexits)
        | Seq(p, q) ->
            let k = {pc=entry.pc;value=cnt()}
            let pnodes, pexits = (vs parent entry k p lbl)
            let qnodes, qexits = (vs (parentUnion pexits) k exit q lbl)
            (Set.union pnodes qnodes), qexits
        | Par(p, q) ->
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter -1, makeCounter -1
            let newPar = parent.Add entry
            let pnodes, pexits = visit name rootEntry lCount newPar {pc=leftPc;value=lCount()} {pc=leftPc;value=lCount()} p (lbl + "_L")
            let qnodes, qexits = visit name rootEntry rCount newPar {pc=rightPc;value=rCount()} {pc=rightPc;value=rCount()} q (lbl + "_R")
            (Set.union pnodes qnodes), (Set.union pexits qexits).Add(entry)
        | Skip -> Set.singleton <| Goto(parent, entry, exit, lbl), Set.singleton <| exit
        | Nil -> Set.singleton <| Stop(parent, entry), Set.empty

    let pc = pccount()
    let entry = {pc=pc;value=counter()}
    (visit 
        rootName entry counter Set.empty entry 
        {pc=pc;value=counter()} (procs.[rootName] ^. Nil) rootName
    |> fst), entry.value

let encode (sys, mapping) = 
    let spawnedComps = 
        sys.components
        |> Map.filter (fun n _ -> sys.spawn.ContainsKey n)
    let counter = makeCounter(-1)

    let trees = 
        spawnedComps
        |> Map.map (fun _ def -> (def, Map.merge sys.processes def.processes))
        |> Map.map (fun _ (def, procs) -> baseVisit procs counter mapping def.behavior)

    Result.Ok(sys, trees, mapping)
    
let translateHeader ((sys,trees, mapping:KeyMapping), bound) =
    // Find the number of PCs used in the program
    let maxPc =
        let rec getPc = function
            | Goto(entry=e; parent=p)
            | Stop(entry=e; parent=p)
            | Basic(entry=e; parent=p) -> 
                if p.IsEmpty then
                    e.pc
                else
                    (p |> Set.map (fun x -> x.pc) |> Set.maxElement)
                    |> max e.pc
            | Guarded(_, n) -> (getPc n)
        trees
        |> Map.values
        |> Seq.map fst
        |> Set.unionMany
        |> Set.map (getPc)
        |> Set.maxElement
        |> (+) 1

    let maxcomps = 
        Map.fold (fun state k (_, cmax) -> max state cmax) 0 sys.spawn

    let defines = 
        [
        "BOUND", bound; 
        "MAXCOMPONENTS", maxcomps;
        "MAXPC", maxPc; 
        "MAXKEYI", ((findMaxIndex I mapping) + 1)
        "MAXKEYL", ((findMaxIndex L mapping) + 1)
        "MAXKEYE", ((findMaxIndex E mapping) + 1)
        ]
        |> List.map (fun (a,b) -> Dict ["name", Str a; "value", Int b])

    [
        "defines", Lst defines;
        "link", Str (translateLink mapping sys.link)
    ]
    |> renderFile "templates/header.c"
    |> Result.bind (fun () -> Result.Ok(sys, trees, mapping))



let translateAll (sys, trees, mapping:KeyMapping) =
    let doOffset o =
        match o with
        | Some e -> translateExpr mapping e
        | None -> "0"
        
    let liquid a =
        let template, (k:Ref<Var>), e =
            match a with
            | AttrUpdate(k, e) -> "attr", k, e
            | LStigUpdate(k, e) -> "lstig", k, e
            | EnvWrite(k, e) -> "env", k, e

        let _, index = mapping.[k.var.name]
        let size = match k.var.vartype with Array(s) -> s | _ -> 0

        [
            "labs", Str (a.ToString())
            "type", Str template;
            "key",  Int index;
            "offset", doOffset k.offset |> Str
            "size", Int size
            "expr", translateExpr mapping e |> Str
            "qrykeys", Lst (
                getLstigVars e 
                |> Set.map (fun v -> snd mapping.[v.name])
                |> Seq.map Int);
        ]

    let rec newTranslate guards n = 
        let entries (node:Node) =
            node.entrypoints
            |> Seq.map (fun x -> Dict ["pc", Int x.pc; "value", Int x.value]) 

        let exitHash e = 
            seq [
                "exitpc", Int e.pc;
                "exitvalue", Int e.value;
            ]

        let template, list = 
            match n with
            | Guarded(s, node) -> newTranslate (s+guards) node
            | Basic(parent, entry, a, exit, lbl) ->
                transition, Seq.append (exitHash exit) (liquid a)
            | Goto(parent, entry, exit, lbl) ->
                goto, (exitHash exit)
            | Stop(parent, entry) ->
                stop, Seq.empty
        template,
        [
            "label", Str n.lbl;
            "entrypoints", (entries n) |> Lst ;
            "guards", Str guards
        ] |> Seq.append list

    trees
    |> Map.values
    |> Seq.map fst
    |> Set.unionMany
    |> Seq.map (fun n -> newTranslate "" n)
    |> Seq.map (fun (t, list) -> t |> Result.bind (render list))
    /// This will be a Result.Ok iff. all results in the sequence are Ok
    |> Seq.fold (fun state y -> Result.bind (fun () -> y) state) (Result.Ok ())
    |> Result.bind(fun () -> Result.Ok(sys, trees, mapping))


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
    |> Result.bind (fun () -> Result.Ok(sys, trees, mapping))