module Encode
open Types
open Base
open Templates
open Expressions


/// A Node is composed by a program counter identifier, an entry/exit point,
/// and some contents: either an action or a pair of parallel nodes.
/// Nodes can be augmented by guards.
type Node = 
    | Basic of pc:int * entry:int * Action * exit:int * lbl:string
    | Guarded of BExpr * Node
    | Parallel of pc:int * lpc:int * rpc:int * entry:int * Set<Node> * exit:int
    | Goto of int * int * int * lbl:string
    | Stop of int * int

let rec getEntryPoint pc = function
    | Basic(pc=p; entry=e) when p=pc -> Some e
    | Parallel(pc=p; entry=e) when p=pc -> Some e
    | Goto(p,e,_,_) when p=pc -> Some e
    | Guarded(_, n) -> getEntryPoint pc n
    | _ -> None

/// Returns the exit point of a node if is has program counter pc.
let rec getExitPoint pc  = function
| Basic(pc=p; exit=e) when p=pc -> Some e
| Parallel(pc=p; exit=e) when p=pc -> Some e
| Goto(p,_,e,_) when p=pc -> Some e
| Guarded(_, n) -> getEntryPoint pc n
| _ -> None

/// Finds the "last" nodes in a set, i.e. those with the highest entry point.
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

let private makeCounter (init: int) =
    let x = ref init
    let incr() =
        x := !x + 1
        !x
    incr


let enumerate s = 
    s
    |> Seq.mapi(fun i x -> x,i)
    |> Map.ofSeq

let joins = ref Set.empty
let encode (sys,(spawn: Map<string,int>)) = 
    let count = makeCounter(-1)
    let pccount = makeCounter(-1)

    let rec visit (processes:Map<string, Process>) procName procPc pc cnt entry exit p lbl =

        let visitPar pc i j p1 p2 =
            // Count 2 new program counters
            let leftPc, rightPc = pccount(), pccount()
            let lCount, rCount = makeCounter(-1), makeCounter(-1)
            let left =
                visit processes procName procPc leftPc lCount (lCount()) (lCount()) p1 (lbl+"_L")
                |> Set.ofSeq
            visit processes procName procPc rightPc rCount (rCount()) (rCount()) p2 (lbl+"_R")
                |> Set.ofSeq
                |> Set.union left
                |> (fun nodes -> 
                    let exitPoints = 
                        (lastNodes leftPc nodes) 
                        |> Set.map (fun x -> pc, exit, leftPc, (getExitPoint leftPc x))
                        |> Set.union (
                            (lastNodes rightPc nodes) 
                            |> Set.map (fun x -> pc, exit, rightPc, (getExitPoint rightPc x)))
                    joins := Set.union exitPoints (!joins)
                    printf "%A" !joins
                    Parallel(pc, leftPc, rightPc, i, nodes, j))
                |> Set.singleton

        let vs = visit processes procName procPc
        match p with
        | Base(a) -> Set.singleton <| Basic(pc, entry, a, exit, lbl)
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
        visit processes procName i (pccount()) count i (count()) processes.[procName] ""

    let spawnedComps = 
        sys.components
        |> Map.filter (fun n _ -> spawn.ContainsKey n)
    let trees = 
        spawnedComps
        |> Map.map (fun _ def -> (def, Map.merge sys.processes def.processes))
        |> Map.map (fun _ (def, procs) -> rootVisit procs def.behavior)
    printfn "%A" trees
    Result.Ok(sys, trees, pccount(), spawn)

let translateHeader (sys,trees,maxPc,spawn) =
    printfn "%s" (globals maxPc);
    let ranges = 0

    let attrKeys = 
        sys.components
        |> Map.values
        |> Seq.map (fun c -> Map.keys c.iface)
        |> Seq.map (Set.map (fun k -> (k, I))) 
        |> Set.unionMany
    let lstigKeys = 
        sys.components
        |> Map.values
        |> Seq.map (fun c -> Map.keys c.lstig)
        |> Seq.map (Set.map (fun k -> (k, L))) 
        |> Set.unionMany
    // Assign a unique id to each attribute/lstig/environment key
    let mapping = 
        attrKeys
        |> Set.union lstigKeys
        |> Set.union <| Set.map (fun k -> (k, E)) sys.environment
        |> enumerate


    sys.spawn
    |> Map.map (fun x num -> num, sys.components.[x].iface)
    |> Map.map (fun _ (num, x) -> 
        num, x
        |> Map.map (fun k v -> 
            init "comp[i].I" mapping.[k,I] v)
        |> Map.values
        |> String.concat "\n"
    )
    //|> Map.map (fun _ (num, x) -> forLoop 0 1 x)
    |> Map.fold (fun (count, str) _ (num, inits) -> 
        let newCount = count + num
        (newCount, str + (forLoop count newCount inits)))
        (0, "")
    |> snd
    |> printfn "%s"
        //|> String.concat "\n"
    //printfn "%s" a
    

    Result.Ok(sys, trees, maxPc, spawn, mapping)

let translateAll (sys,trees,maxPc,spawn,mapping:Map<(string * TypeofKey),int>) =

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

    let rec translateNode parentEntry parentExit name = function
    | Basic(pc, entry, AttrUpdate(key, e), exit, lbl) ->
        cvoid (signature name entry lbl) "int tid"
            (parentEntry + (entrypoint pc entry) + (entryJoins pc entry) +
            (attr (translateExpr mapping e)(mapping.[key,I])) +
            (updateKq <| lstigKeys mapping e) +
            (exitJoins pc entry) + (exitpoint pc exit) + parentExit)
    | Guarded(b, node) -> 
        translateNode (parentEntry+(assume (translateBExpr mapping b))) parentExit name node
    | Parallel(pc, lpc, rpc, entry, nodes, exit) -> 
        //(join name lpc rpc pc entry exit) +
        (nodes 
        |> Set.map (fun node ->
            let last = Set.union (lastNodes lpc nodes) (lastNodes rpc nodes)
            let exit = if last.Contains node then (exitpoint pc exit) else ""
            translateNode (parentEntry+(entrypoint pc entry)) (parentExit+exit) name node)
        |> String.concat "\n")
    | Goto(pc, entry, exit, lbl) ->  
        cvoid (signature name entry lbl) "int tid" 
            (parentEntry + (entrypoint pc entry) +
            (exitpoint pc exit)+parentExit)
    | _ -> ""

    trees
    |> Map.map (fun x y -> y |> Set.map (translateNode "" "" x))
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (String.concat "\n")
    |> String.concat "\n"
    |> printfn "%s"

    Result.Ok(sys, trees)