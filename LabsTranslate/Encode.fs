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
| Basic of 
    guards:Set<BExpr<Var, unit>> *
    parent:Set<pcCondition> * 
    entry:pcCondition *
    Action<Var> * 
    exit:pcCondition * 
    lbl:string
| Goto of guards:Set<BExpr<Var, unit>> * parent:Set<pcCondition> * entry:pcCondition * exit:pcCondition * lbl:string
| Stop of guards:Set<BExpr<Var, unit>> * parent:Set<pcCondition> * entry:pcCondition
with 
    member this.lbl = 
        match this with
        | Basic(lbl=l;entry=e)
        | Goto(lbl=l;entry=e) -> sprintf "%s_%i" l e.value
        | Stop(entry=e) -> sprintf "Stop_%i" e.value
        //| Guarded(_,n) -> n.lbl
    member this.entrypoints = 
        match this with
        //| Guarded(_,n) -> n.entrypoints
        | Basic(parent=par;entry=e)
        | Goto(parent=par;entry=e)
        | Stop(parent=par;entry=e) ->
            let x = Set.filter (fun p -> p.pc = e.pc) par
            if x.IsEmpty then Set.add e par else par

let baseVisit (procs:Map<string, Process<Var>>) counter mapping rootName =
    let pccount = makeCounter -1
    let rec visit name rootEntry cnt guards parent entry exit p lbl =
        let vs = visit name rootEntry cnt guards
        let parentUnion = Set.union parent

        match p with
        | Base(a) -> 
            Set.singleton <| Basic(guards, parent, entry, a, exit, lbl)
        | Name(s) when s = name ->
            Set.singleton <| Goto(guards, parent, entry, rootEntry, lbl)
        | Name(s) -> visit name rootEntry cnt guards parent entry exit procs.[s] (lbl)
        | Await(b, p) -> 
            visit name rootEntry cnt (guards.Add b) parent entry exit p lbl
        | Choice(p, q) -> 
            let pnodes = (vs parent entry exit p (lbl+"_L"))
            let qnodes = (vs parent entry exit q (lbl+"_R"))
            (Set.union pnodes qnodes)
        | Seq(p, q) ->
            let k = {pc=entry.pc;value=cnt()}
            let pnodes = vs parent entry k p lbl
            let qnodes =
                visit name rootEntry cnt Set.empty Set.empty k exit q lbl
            (Set.union pnodes qnodes)
        | Par(p, q) ->
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter -1, makeCounter -1
            let newPar = parent.Add entry
            let pnodes = visit name rootEntry lCount guards newPar {pc=leftPc;value=lCount()} {pc=leftPc;value=lCount()} p (lbl + "_L")
            let qnodes = visit name rootEntry rCount guards newPar {pc=rightPc;value=rCount()} {pc=rightPc;value=rCount()} q (lbl + "_R")
            (Set.union pnodes qnodes)//, (Set.union pexits qexits).Add(entry)
        | Skip -> Set.singleton <| Goto(guards, parent, entry, exit, lbl)
        | Nil -> Set.singleton <| Stop(guards, parent, entry)

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
        |> Map.map (fun _ def -> (def, Map.merge sys.processes def.processes))
        |> Map.map (fun _ (def, procs) -> baseVisit procs counter mapping "Behavior")
        //|> Map.mapValues fst

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
        trees
        |> Map.values
        |> Seq.map fst
        |> Set.unionMany
        |> Set.map (getPc)
        |> Set.maxElement
        |> (+) 1

    let maxcomps = 
        Map.fold (fun state k (_, cmax) -> max state cmax) 0 sys.spawn

    let ifaces = mapping |> Map.filter (fun _ (v, _) -> v.location = I)
    let env = mapping |> Map.filter (fun _ (v, _) -> v.location = E)
    let lstig =
        mapping
        |> Map.filter (fun _ (v, _) -> match v.location with L(_) -> true | _ -> false)

    let defines = 
        [
            "BOUND", bound; 
            "MAXCOMPONENTS", maxcomps;
            "MAXPC", maxPc; 
            "MAXKEYI", ((findMaxIndex ifaces) + 1)
            "MAXKEYL", ((findMaxIndex lstig) + 1)
            "MAXKEYE", ((findMaxIndex env) + 1)
        ]
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
                "link", Str (translateLink mapping s.link)
            ] |> Some

        sys.stigmergies
        |> Map.values
        |> Seq.choose makeLink

    [
        "defines", Lst defines
        "links", Lst links
    ]
    |> renderFile "templates/header.c"
    |> Result.bind (fun () -> Result.Ok(sys, trees, mapping))



let translateAll (sys, trees, mapping:KeyMapping) =
    let doOffset  = function
        | Some e -> translateExpr mapping e |> Str
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
            "expr", translateExpr mapping e |> Str
            "qrykeys",
                getLstigVars e 
                |> Seq.map (fun v -> snd mapping.[v.name] |> Int)
                |> Lst
        ]

    let rec newTranslate n = 
        let entries (node:Node) =
            node.entrypoints
            |> Seq.map (fun x -> Dict ["pc", Int x.pc; "value", Int x.value]) 

        let exitHash e = 
            seq [
                "exitpc", Int e.pc;
                "exitvalue", Int e.value;
            ]

        let template, guards, list = 
            match n with
            //| Guarded(s, node) -> newTranslate (s+guards) node
            | Basic(guards, parent, entry, a, exit, lbl) ->
                transition, guards, Seq.append (exitHash exit) (liquid a)
            | Goto(guards, parent, entry, exit, lbl) ->
                goto, guards, (exitHash exit)
            | Stop(guards, parent, entry) ->
                stop, guards, Seq.empty
        
        template,
        [
            "label", Str n.lbl;
            "entrypoints", (entries n) |> Lst ;
            "guards",
                guards
                |> Set.map (assume << translateGuard mapping)
                |> Seq.map Str
                |> Lst
        ] |> Seq.append list

    trees
    |> Map.values
    |> Seq.map fst
    |> Set.unionMany
    |> Seq.map newTranslate
    |> Seq.map (fun (t, list) -> t |> Result.bind (render list))
    /// This will be a Result.Ok iff. all results in the sequence are Ok
    |> Seq.fold (fun state y -> Result.bind (fun () -> y) state) (Result.Ok ())
    |> Result.bind (fun () -> Result.Ok(sys, trees, mapping))


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