module Encode
open Types
open Base

type Node = 
    //| Inner of int * Process * int
    | Check of int * BExpr * int
    | Leaf of int * Action * int
    | Goto of int * int
    | Stop of int

let makeCounter (init: int) =
    let x = ref init
    let incr() =
        x := !x + 1
        !x
    incr

/// Assigns a unique int to each key in the program.
let enumerateKeys processes =
    let rec allKeysExpr = function
    | K(x) -> Set.singleton x
    | Sum(e1, e2) -> Set.union (allKeysExpr e1) (allKeysExpr e2)
    | _ -> Set.empty

    let rec allKeysBexpr = function
    | Compare(e1, _, e2) -> Set.union (allKeysExpr e1) (allKeysExpr e2)
    | NilCheck(e) -> allKeysExpr e
    | Neg(b) -> allKeysBexpr b
    | Conj(b1, b2) -> Set.union (allKeysBexpr b1) (allKeysBexpr b2)
    | _ -> Set.empty

    let rec allKeys = function
    | Base(AttrUpdate(a, e))
    | Base(LStigUpdate(a, e))
    | Base(EnvWrite(a, e)) -> (allKeysExpr e).Add a
    | Base(EnvRead(a, b)) -> (Set.singleton a).Add b
    | Par(p, q) 
    | Choice(p, q)
    | Seq(p, q) -> Set.union (allKeys p) (allKeys q)
    | Await(b, p) -> Set.union (allKeysBexpr b) (allKeys p)
    | Name(_)
    | Nil
    | Skip -> Set.empty

    processes
    |> List.map allKeys
    |> Set.unionMany
    |> Seq.mapi (fun i x -> x,i)
    |> Map.ofSeq




//let makeTree (processes: Map<string, Process>) p =
    //let pcProcesses = 
    //    processes
    //    |> Map.toSeq
    //    |> fstSet
    //    |> Seq.mapi (fun i x -> x,i)
    //    |> Map.ofSeq

    //let maxPc = Map.fold (fun x _ pc -> max x pc) 0 pcProcesses
    //let count = makeCounter(maxPc)


    //let rec visit i j = function
    //    | Base(a) -> Set.singleton <| Leaf(i, a, j)
    //    | Seq(p,q) -> 
    //         let k = count()
    //         Set.union (visit i k p) (visit k j q)
    //    | Await(b, p) ->
    //         let k = count()
    //         (visit k j p).Add(Check(i, b, k))
    //    | Choice(p, q) ->
    //        Set.union (visit i j p) (visit i j q)
    //    | Name(s) ->
    //        Set.singleton <| Skip(i, (pcProcesses.Item s))
    //    | Tick -> Set.singleton <| Skip(i, j)
    //    | Nil -> Set.singleton <| Stop i
    //    | Par(p,q) ->

    //visit (count()) (count()) p
 
