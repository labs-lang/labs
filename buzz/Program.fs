// Learn more about F# at http://fsharp.org

open System
open System.IO
open Buzz.Types
open Buzz.Functions
open Buzz.Component
open Buzz.Json
open Buzz.System
open Chiron

let print x =
    printfn "----\n%A" (x)
    x
   

let triangle lim n = 
    let twiceLim = lim * 2
    let x = modulo n twiceLim
    if x <= lim then x else twiceLim-x 
let trPoint triangleX triangleY (x,y) =
    (triangleX x, triangleY y)

let changeDir changePoint (dx,dy) (x,y) =
    let (x', y') = changePoint (x,y)
    ((if x-x' <> 0 then -dx else dx), if y-y' <> 0 then -dy else dy)

let bounce xMax yMax (c:Comp) =
    let updatePoint = trPoint (triangle xMax) (triangle yMax)

    let loc = c.I.["loc"]
    let dir = c.I.["dir"]
    match (loc, dir) with
    | P(l), P(d) -> 
    {c with I = c.I.Add("loc", P(updatePoint l)).Add("dir", P(changeDir updatePoint d l))}
    | _ -> c

let bounceL xMax yMax c =
    let c' = bounce xMax yMax c
    if c'.I.["dir"] <> c.I.["dir"]
    then {c' with P = LazyPut("dir", I("dir"))^.c'.P}
    else c'

/// n components move in random directions. When two component "meet", they
/// start moving in the same direction.
[<EntryPoint>]
let swarm argv = 
    let proc = LazyPut("dir", I("dir")) ^. RecX(Attr("loc", Sum(I("loc"),L("dir"))) ^. X)
    let n = 10
    let xMax, yMax = 50, 50


    let directions = seq {for x in [1..n] do yield randomDirection()}
    let points = seq {for x in [1..n] do yield P(rng.Next(xMax+1), rng.Next(yMax+1))}

    let sys = 
        Seq.map2 (fun d p -> [("loc", p); ("dir", d)] |> Map.ofSeq) directions points
        |> Seq.map (fun i -> {Comp.Create() with I = i; P=proc})
        |> Set.ofSeq
    
    sys
    |> sysToJson
    |> Json.format
    |> print
    |> ignore

    use streamWriter = new StreamWriter("swarm.json", false)

    //run sys (KeyConsensus ["dir"]) [(TT, (bounceL xMax yMax))]
    run sys (NumberOfSteps 500) [(TT, (bounceL xMax yMax))]
    |> Seq.toList
    |> traceToJson
    |> Json.format
    |> streamWriter.WriteLine 

    0
