// Learn more about F# at http://fsharp.org

open System
open System.IO
open Buzz.Types
open Buzz.Functions
open Buzz.Component
open Buzz.Json
open Buzz.LStig
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
    let (xNew, yNew) = changePoint (x,y)
    ((if x-xNew <> 0 then -dx else dx), if y-yNew <> 0 then -dy else dy)

let bounce xMax yMax (c:Comp) =
    let updatePoint = trPoint (triangle xMax) (triangle yMax)
    let loc =
        match c.I.TryFind("loc") with
        | Some(P(l)) -> l
        | _ -> failwith <| sprintf "Component %A has no 'loc' attribute." c

    let newC = {c with I = c.I.Add("loc", P(updatePoint loc))}

    match newC.I.TryFind("dir") with
    | Some(P(d)) -> {newC with I = newC.I.Add("dir", P(changeDir updatePoint d loc))}
    | _ -> newC

let bounceL xMax yMax c =
    let newC = bounce xMax yMax c
    if newC.L.TpairOf("dir").IsSome then
        match newC.I.["dir"], c.I.["dir"] with
        | d1, d2 when d1 <> d2 -> {newC with P = LazyPut("dir", I("dir"))^. newC.P}
        | _ -> newC
    else newC

/// n components move in random directions. When two component "meet", they
/// start moving in the same direction.
let swarm n xMax yMax = 
    let p1 = Attr("loc", Sum(I("loc"), RandomPoint(-1,-1,1,1))) ^. X
    let p2 = 
        Attr("dir", RandomPoint(-1,-1,1,1)) ^. 
        LazyPut("dir", I("dir")) ^. 
        RRec(Attr("loc", Sum(I("loc"),L("dir"))) ^. X)


    let proc = RecX((Await("ok", Int(1)) ^. p1) + (AwaitNot("ok", Int(1)) ^. p2))

    let directions = seq {for x in [1..n] do yield randomDirection()}
    let points = seq {for x in [1..n] do yield P(rng.Next(xMax+1), rng.Next(yMax+1))}
    let lstig = LStig.Empty + ("ok", (Int(1), DateTime.Now))

    Seq.map2 (fun d p -> [("loc", p); ("dir", d)] |> Map.ofSeq) directions points
    |> Seq.map (fun i -> {Comp.Create() with I = i; P = proc; L = lstig})
    |> Set.ofSeq

let update k v c = 
    {c with L = c.L + (k, (v, DateTime.Now))}

[<EntryPoint>]
let main argv =
    let xMax, yMax = 49, 49
    let sys = swarm 10 xMax yMax

    let link = distanceLink 5.0
    let setOkToZero = update "ok" <| Int(0)

    sys
    |> sysToJson
    |> Json.format
    |> print
    |> ignore
   
    use streamWriter = new StreamWriter("swarm.json", false)
    //run sys () [(TT, (bounceL xMax yMax))]
    run sys link (KeyConsensus ["dir"]) [(TT, (bounceL xMax yMax)); (AtStep 300, setOkToZero)]
    |> Seq.toList
    |> (fun x -> print x.Length |> ignore; x)
    |> traceToJson
    |> Json.format
    |> streamWriter.WriteLine 


    0