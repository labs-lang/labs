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

let private makeCounter (start: int) =
    let x = ref start
    let incr() =
        x := !x + 1
        !x
    incr

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
                    //joins := Set.union exitPoints (!joins)
                |> fun nodes -> Parallel(pc, leftPc, rightPc, i, nodes, j)
                |> Set.singleton

        let vs = visit processes procName procPc
        match p with
        | Base(a) -> Set.singleton <| Basic(pc, entry, a, exit, (signature lbl entry ""))
        | Seq(p, Name(s)) when s = procName -> 
            vs pc cnt entry procPc p lbl
        | Seq(p,q) -> 
             let k = cnt()
             Set.union (vs pc cnt entry exit p lbl) (vs pc cnt exit k q lbl)
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

let translateHeader (((sys,trees,maxPc), mapping:KeyMapping), bound) =
    let maxcomps = 
        Map.fold (fun state k (_, cmax) -> max state cmax) 0 sys.spawn
    let maxkey = 
        Map.fold (fun state k (info) -> max state info.index) 0 mapping

    printfn "#define BOUND %i" bound
    printfn "#define MAXPROCS %i" maxcomps
    printfn "#define MAXPC %i" maxPc
    printfn "#define MAXKEY %i" maxkey
    printfn "%s" baseHeader
    printfn "%s" (encodeLink mapping sys.link)
    printfn "%s" systemFunctions


    let makeInits initMap = 

        initMap
        |> Map.map (fun k v -> init mapping.[k] v)
        |> Map.values
        |> String.concat "\n"

    sys.spawn
    |> Map.map (fun x range -> range, (makeInits sys.components.[x].iface))
    |> Map.map (fun x (r, inits) -> (r, inits+"\n"+makeInits sys.components.[x].lstig))
    |> Map.fold (fun str _ ((rangeStart, rangeEnd), inits) -> 
        (str + (forLoop rangeStart rangeEnd inits))) ""
    |> baseInit
    |> (indent 4)
    |> (cvoid "init" "")
    |> printfn "%s"

    Result.Ok(sys, trees, maxPc, mapping)

let translateAll (sys,trees,maxPc,mapping:KeyMapping) =

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

    let rec translateNode parentEntry parentExit = function
    | Basic(pc, entry, AttrUpdate(key, e), exit, lbl) -> // TODO Lstig, Env
        let info = getInfoOrFail mapping key
        cvoid lbl "int tid" (indent 4
                (parentEntry + (entrypoint pc entry) + (entryJoins pc entry) +
                    (attr (translateExpr mapping e)(info.index)) +
                    (updateKq <| getLstigKeys mapping e) +
                    (exitJoins pc entry) + (exitpoint pc exit) + parentExit))
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

    let makeIf (info:string*Set<int*int>) = 
        info
        |> snd
        |> Set.map (fun (pc, entry) -> sprintf "pc[choice[i]][%i]==%i" pc entry)
        |> String.concat " && "
        |> fun x -> sprintf "if (%s) %s(choice[i]);" x (fst info)

    let nodeInfo =  
        trees
        |> Map.values 
        |> Set.unionMany
        |> Set.map (getAllEntryPoints Set.empty)
        |> Set.unionMany
    let first = Set.minElement nodeInfo

   
    let elseblock = 
        nodeInfo
        |> Set.remove first
        |> Set.map (fun n -> "else " + (makeIf n))
        |> String.concat "\n"
    printfn "%s"
        (tmain 
            typeofInterleaving
            ((makeIf first) + "\n" + elseblock)
            (translateFinallyProperties sys mapping))


    Result.Ok(sys, trees, mapping)
