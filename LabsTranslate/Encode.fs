module Encode
open Types
open Base
open Templates
open Expressions
open Properties
open Link

/// A Node is composed by a program counter identifier, an entry/exit point,
/// and some contents: either an action or a pair of parallel nodes.
/// Nodes can be augmented by guards.
type Node = 
| Basic of pc:int * entry:int * Action * exit:int * lbl:string
| Guarded of BExpr * Node
| Parallel of pc:int * lpc:int * rpc:int * entry:int * nodes:Set<Node> * exit:int
| Goto of pc:int * entry:int * int * lbl:string
| Stop of int * int

let rec getEntryPoint pc = function
| Basic(pc=p; entry=e) when p=pc -> Some e
| Parallel(pc=p; entry=e) when p=pc -> Some e
| Goto(p,e,_,_) when p=pc -> Some e
| Guarded(_, n) -> getEntryPoint pc n
| _ -> None

let rec getAllEntryPoints (par:Set<int*int>) = function
| Basic(pc=p; entry=e; lbl=l)
| Goto(pc=p; entry=e; lbl=l) ->
    (l, par.Add (p, e)) |> Set.singleton
| Parallel(pc=p; entry=e; nodes=ns) -> 
    ns
    |> Set.map (fun n -> getAllEntryPoints (par.Add (p, e)) n)
    |> Set.unionMany
| Guarded(_, n) -> getAllEntryPoints par n
| Stop(_) -> Set.empty

/// Returns the exit point of a node if is has program counter pc.
let rec getExitPoint pc  = function
| Basic(pc=p; exit=e) when p=pc -> Some e
| Parallel(pc=p; exit=e) when p=pc -> Some e
| Goto(p,_,e,_) when p=pc -> Some e
| Guarded(_, n) -> getEntryPoint pc n
| _ -> None

/// Finds the last nodes in a set, i.e. those with the highest entry point.
let lastNodes pc nodes =
    let rec hasPc m = function
    | Basic(pc=p; entry=e) when (p,e)=(pc,m) -> true
    | Guarded(_, n) -> hasPc m n
    | Basic(pc=p; entry=e) when (p,e)=(pc,m) -> true
    | Parallel(pc=p; entry=e) when (p,e)=(pc,m) -> true
    | Goto(p,e,_,_) when (p,e)=(pc,m) -> true
    | _ -> false

    let maxValue = nodes |> Seq.choose (getEntryPoint pc) |> Seq.max
    nodes |> Set.filter (hasPc maxValue)
    //let exitPoints = last |> Seq.choose (getExitPoint pc)
    //last


/// Set of execution point where a join happens
let private joins = ref Set.empty

let encode (sys) = 
    let count = makeCounter(-1)
    let pccount = makeCounter(-1)

    let rec visit (processes:Map<string, Process>) procName procPc pc cnt entry exit p lbl =

        let visitPar pc i j p1 p2 =
            // Make 2 new program counters
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter(-1), makeCounter(-1)
            let left =
                visit processes procName procPc leftPc lCount (lCount()) (lCount()) p1 (lbl+"_L")
                |> Set.ofSeq
            visit processes procName procPc rightPc rCount (rCount()) (rCount()) p2 (lbl+"_R")
                |> Set.ofSeq
                |> Set.union left
                |> fun nodes -> 
                    // Keep track of the exit points of the parallel process
                    (lastNodes leftPc nodes) 
                    |> Set.map (fun x -> pc, exit, leftPc, (getExitPoint leftPc x))
                    |> Set.union (
                        (lastNodes rightPc nodes) 
                        |> Set.map (fun x -> pc, exit, rightPc, (getExitPoint rightPc x)))
                    |> Set.union !joins
                    |> (:=) joins
                    nodes
                |> fun nodes -> Parallel(pc, leftPc, rightPc, i, nodes, j)
                |> Set.singleton

        let vs = visit processes procName procPc
        match p with
        | Base(a) -> Set.singleton <| Basic(pc, entry, a, exit, (signature lbl entry ""))
        | Seq(p, Name(s)) when s = procName -> 
            vs pc cnt entry procPc p lbl
        | Seq(p,q) -> 
             let k = cnt()
             Set.union (vs pc cnt entry k p lbl) (vs pc cnt k exit q lbl)
        | Await(b, p) -> (vs pc cnt entry exit p lbl) |> Set.map (fun n -> Guarded(b, n))
        | Choice(p, q) ->
            Set.union (vs pc cnt entry exit p (lbl+"_L")) (vs pc cnt entry exit q (lbl+"_R"))
        | Name(s) when s = procName ->
            Set.singleton <| Goto(pc, entry, procPc, lbl)
        | Name(s) -> visit processes s entry pc cnt entry exit (processes.[s]) lbl 
        | Skip -> Set.singleton <| Goto(pc, entry, exit, lbl)
        | Nil -> Set.singleton <| Stop (pc, entry)
        | Par(p,q) -> (visitPar pc entry exit p q)
    let rootVisit (processes: Map<string, Process>) procName =
        let i = count()
        visit processes procName i (pccount()) count i (count()) processes.[procName] procName

    let spawnedComps = 
        sys.components
        |> Map.filter (fun n _ -> sys.spawn.ContainsKey n)
    let trees = 
        spawnedComps
        |> Map.map (fun _ def -> (def, Map.merge sys.processes def.processes))
        |> Map.map (fun _ (def, procs) -> rootVisit procs def.behavior)
    Result.Ok(sys, trees, pccount())

let makeTuples comps mapping =
    let lstigKeys = Map.filter (fun k info -> info.location = L) mapping

    let tupleExtrema key =
        let filtered (m:Map<Key, 'a>) =
            lstigKeys
            |> Map.filter (fun k _ -> m.ContainsKey k)

        comps
        |> Seq.map (fun c -> 
            c.lstig
            |> Seq.filter (fun m -> m.ContainsKey key)
            |> Seq.map (fun m -> 
                if m.Count > 1
                then (findMinIndex L (filtered m), findMaxIndex L (filtered m))
                else (mapping.[key].index, mapping.[key].index)))
        |> Seq.concat
        |> Seq.head

    lstigKeys
    |> Map.map (fun k info ->
        let extrema = tupleExtrema k
        (sprintf """
tupleStart[%i] = %i;
tupleEnd[%i] = %i;
""" info.index (fst extrema) info.index (snd extrema)))
    |> Map.values
    |> String.concat ""

let translateHeader (((sys,trees,maxPc), mapping:KeyMapping), bound) =
    let maxcomps = 
        Map.fold (fun state k (_, cmax) -> max state cmax) 0 sys.spawn

    printfn "#define BOUND %i" bound
    printfn "#define MAXPROCS %i" maxcomps
    printfn "#define MAXPC %i" maxPc
    printfn "#define MAXKEYI %i" ((findMaxIndex I mapping) + 1)
    printfn "#define MAXKEYL %i" ((findMaxIndex L mapping) + 1)
    printfn "#define MAXKEYE %i" ((findMaxIndex E mapping) + 1)
    printfn "%s" baseHeader
    printfn "%s" (encodeLink mapping sys.link)
    printfn "%s" systemFunctions
    Result.Ok(sys, trees, mapping)

let translateInitSim (sys,trees, mapping:KeyMapping) =
    let makeInits i initMap = 
        initMap
        |> Map.map (fun k v -> initSimulate i mapping.[k] v)
        |> Map.values
        |> String.concat "\n"
    sys.spawn
    |> Map.map (fun x (minI, maxI) -> 
        seq [minI..maxI-1]
        |> Seq.map (fun i -> 
            (makeInits i sys.components.[x].iface) + "\n" +
            (sys.components.[x].lstig
             |> Seq.map (makeInits i)
             |> String.concat "\n"))
        |> String.concat "\n")
    |> Map.values
    |> String.concat "\n"
    |> 
        if not sys.environment.IsEmpty then 
            (+) (makeInits 0 sys.environment)
        else id
    |> baseInit
    |> (+) (makeTuples (Map.values sys.components) mapping)
    |> (indent 4)
    |> (cvoid "init" "")
    |> printfn "%s"

    Result.Ok(sys, trees, mapping)

let translateInit (sys,trees, mapping:KeyMapping) =
    let makeInits initMap = 
        initMap
        |> Map.map (fun k v -> init mapping.[k] v)
        |> Map.values
        |> String.concat "\n"

    sys.spawn
    |> Map.map (fun x range -> range, (makeInits sys.components.[x].iface))
    |> Map.map (fun x (r, inits) -> 
        let lstigsinit = 
            sys.components.[x].lstig
            |> Seq.map makeInits
            |> String.concat "\n"
        (r, inits+ "\n" + lstigsinit))
    |> Map.fold (fun str _ ((rangeStart, rangeEnd), inits) -> 
        (str + (forLoop rangeStart rangeEnd inits))) ""
    |> 
        if not sys.environment.IsEmpty then
            (+) (makeInits sys.environment)
        else id
    |> baseInit
    |> (+) (makeTuples (Map.values sys.components) mapping)
    |> (indent 4)
    |> (cvoid "init" "")
    |> printfn "%s"

    Result.Ok(sys, trees, mapping)

let translateAll (sys, trees, mapping:KeyMapping) =

    let lookupJoins pc entry = 
        (!joins) 
        |> Set.toSeq 
        |> Seq.filter(fun (parentpc, exit, _,_) -> parentpc = pc && exit = entry)

    let entryJoins pc entry = 
        lookupJoins pc entry
        |> Seq.choose (fun (_, _, childPc, v) -> (Option.map (entrypoint childPc) v))
        |> String.concat ""
    let exitJoins pc entry =
        lookupJoins pc entry
        |> Seq.choose (fun (_, _, childPc, v) -> (Option.map (fun _ -> exitpoint childPc 0) v))
        |> String.concat ""

    let encodeAction (a:Action) = 
        let template, k, e = 
            match a with
            | AttrUpdate(k,e) -> attr,k,e
            | LStigUpdate(k,e) -> lstig,k,e
            | EnvWrite(k,e) -> env,k,e
        let info = getInfoOrFail mapping k
        (template (translateExpr mapping e)(info.index)) +
        (updateKq <| getLstigKeys mapping e)

    let rec translateNode parentEntry parentExit = function
    | Basic(pc, entry, a, exit, lbl) -> // TODO Lstig, Env
        cvoid lbl "int tid" (indent 4
                (sprintf "// %s\n   " (a.ToString()) +
                    parentEntry + (entrypoint pc entry) + (entryJoins pc entry) +
                    (encodeAction a) +
                    (exitJoins pc entry) +
                    (exitpoint pc exit) + parentExit))
    | Guarded(b, node) ->  
        translateNode (parentEntry+(assume (translateBExpr mapping b))) parentExit node
    | Parallel(pc, lpc, rpc, entry, nodes, exit) -> 
        (nodes 
        |> Set.map (fun node ->
            let last = Set.union (lastNodes lpc nodes) (lastNodes rpc nodes)
            let exit = if last.Contains node then (exitpoint pc exit) else ""
            translateNode (parentEntry+(entrypoint pc entry)) (parentExit+exit) node)
        |> String.concat "\n")
    | Goto(pc, entry, exit, lbl) ->  
        cvoid (signature lbl entry "") "int tid" 
            (parentEntry + (entrypoint pc entry) +
            (exitpoint pc exit)+parentExit)
    | _ -> ""

    trees
    |> Map.map (fun x y -> y |> Set.map (translateNode "" ""))
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (String.concat "\n")
    |> String.concat "\n"
    |> printfn "%s"

    Result.Ok(sys, trees, mapping)

let translateMain typeofInterleaving (sys,trees:Map<'a, Set<Node>>, mapping) =

    cvoid "monitor" "" (indent 4 (translateAlwaysProperties sys mapping))
    |> printfn "%s"

    let encodeEntry (entrypoints) = 
        entrypoints
        |> Set.map (fun (pc, entry) -> sprintf "pc[choice[i]][%i]==%i" pc entry)
        |> String.concat " && "


    let makeIf (name, entrypoints) = 
        encodeEntry entrypoints
        |> fun x -> sprintf "if (%s) %s(choice[i]);" x name

    let makeNondetIf (names, entrypoints) = 
        names
        |> Seq.mapi (fun i n ->
            if i = 0 then 
                sprintf "if (nondet_bool()) %s(choice[i]);" n
            else if i = (Seq.length names) - 1 then
                sprintf "    else %s(choice[i]);" n
            else
                sprintf "    else if (nondet_bool()) %s(choice[i]);" n
            )
        |> String.concat "\n"
        |> sprintf "if (%s) {\n    %s\n}" (encodeEntry entrypoints)

    let nodeInfo =  
        trees
        |> Map.values 
        |> Set.unionMany
        |> Set.map (getAllEntryPoints Set.empty)
        |> Set.unionMany
    let first = Set.minElement nodeInfo

    let prova =
        nodeInfo
        |> Set.toSeq
        |> Seq.groupBy (fun (name, entry) -> entry)

    let schedule = 
        prova
        |> Seq.map (fun (entry, names) ->
            let ns = names |> Seq.map fst
            if (Seq.length names) = 1 then
                makeIf (Seq.head ns, entry)
            else 
                makeNondetIf(ns, entry)
            )
        |> String.concat "\n"

    let elseblock = 
        nodeInfo
        |> Set.remove first
        |> Set.map (fun n -> "else " + (makeIf n))
        |> String.concat "\n"
    printfn "%s"
        (tmain 
            typeofInterleaving
            schedule
            (translateFinallyProperties sys mapping))


    Result.Ok(sys, trees, mapping)
