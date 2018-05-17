module Encode
open Types
open Base
open Templates


/// A Node is composed by a program counter identifier, an entry/exit point,
/// and some contents: either an action or a pair of parallel nodes.
/// Nodes can be augmented by guards.
type Node = 
    | Basic of pc:int * entry:int * Action * exit:int * lbl:string
    | Guarded of BExpr * Node
    | Parallel of pc:int * lpc:int * rpc:int * entry:int * Set<Node> * exit:int
    | Goto of int * int * int
    | Stop of int * int


let private makeGenericCounter (init: int) =
    let x = ref init
    let incr(a) =
        x := !x + a
        !x
    incr

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
                |> (fun nodes -> Parallel(pc, leftPc, rightPc, i, nodes, j))
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
            Set.union (vs pc cnt entry exit p (lbl+"_L")) (vs pc cnt entry exit q (lbl+"_L"))
        | Name(s) when s = procName ->
            Set.singleton <| Goto(pc, entry, procPc)
        | Name(s) -> visit processes s entry pc cnt entry exit (processes.[s]) lbl 
        | Skip -> Set.singleton <| Goto(pc, entry, exit)
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

    let cnt = makeGenericCounter
    let a = 
        sys.spawn
        |> Map.map (fun x num -> num, sys.components.[x].iface)
        |> Map.map (fun _ (num, x) -> 
            num, x
            |> Map.map (fun k v -> 
                init "comp[i].I" mapping.[k,I] v)
            |> Map.values
            |> String.concat "\n"
        )
        |> Map.map (fun _ (num, x) -> forLoop 0 1 x)
        |> Map.values
        |> String.concat "\n"
    printfn "%s" a
    

    Result.Ok(sys, trees, maxPc, spawn, mapping)

let translateAll (sys,trees,maxPc,spawn,mapping:Map<(string * TypeofKey),int>) =

    let rec translateExpr = function
        | Const(Int(i)) -> sprintf "%i" i
        //| Const(String(s)) -> "\"" + s + "\""
        | Const(Val.P(p1,p2)) -> ""
        | K(k) when mapping.ContainsKey (k, I)  -> 
            sprintf "comp[tid].I[%i]" mapping.[k,I]
        | K(k) when mapping.ContainsKey (k, L)  -> 
            sprintf "comp[tid].Lvalue[%i]" mapping.[k,L]
        | K(k) when mapping.ContainsKey (k, E)  -> 
            // TODO
            sprintf "E[%i]" mapping.[k,E]
        | K(k) -> failwith "Unexpected key"
        | Sum(e1, e2) -> sprintf "( (%s) + (%s) )" (translateExpr e1) (translateExpr e2)

    let translateBOp = function
    | Less -> "<"
    | Equal -> "=="
    | Greater -> ">"

    let rec translateBExpr = function
    | True -> "true"
    | False -> "false"
    | Neg(b) -> sprintf "!(%s)" (translateBExpr b)
    | Conj(b1, b2) -> sprintf "((%s) && (%s))" (translateBExpr b1) (translateBExpr b2)
    | Compare(e1, op, e2) ->
        sprintf "((%s) %s (%s))" (translateExpr e1) (translateBOp op) (translateExpr e2)


    let rec getLstigKeys = function
    | K(k) when mapping.ContainsKey (k,L) -> (k,L) |> Set.singleton
    | Sum(e1, e2) -> Set.union (getLstigKeys e1) (getLstigKeys e2)
    | _ -> Set.empty

    let lstigKeys expr =
        expr |> getLstigKeys |> Set.map (fun x -> mapping.[x])

    let rec translateNode additional name = function
    | Basic(pc, entry, AttrUpdate(key, e), exit, lbl) ->
        cvoid (signature name entry lbl) "int tid"
            (additional +
            (entrypoint pc entry) +
            (attr (translateExpr e)(mapping.[key,I])) +
            (updateKq <| lstigKeys e) +
            (exitpoint pc exit))
    | Guarded(b, node) -> translateNode (assume (translateBExpr b)) name node
    | Parallel(pc, lpc, rpc, entry, nodes, exit) -> 
        (join name lpc rpc pc entry exit) +
        (nodes 
        |> Set.map (translateNode (additional+(entrypoint pc entry)) name)
        |> String.concat "\n")
    | _ -> ""

    trees
    |> Map.map (fun x y -> y |> Set.map (translateNode "" x))
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (String.concat "\n")
    |> String.concat "\n"
    |> printfn "%s"

    Result.Ok(sys, trees)