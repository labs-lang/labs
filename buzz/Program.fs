// Learn more about F# at http://fsharp.org

open System
open Buzz.Types
open Buzz.Functions
open Buzz.Component
open Buzz.Json
open Buzz.System
open Chiron

let print x =
    printfn "----\n%A" (x)
    x

[<EntryPoint>]
let main argv = 
    printfn "Hello World from F#!"

    // Some basic processes
    let proc = Attr("test", Const(Int(1))) ^. LazyPut("x", I("test")) ^. Nil
    let proc2 = Await("x", Int(1)) ^. LazyPut("y", K("x")) ^. Nil

   
    //let prova = Star(LazyPut("x", Int(1)) ^. Await("x", Int(2)))
    //let prova2 = Star(Await("x", Int(1)) ^. LazyPut("x", Int(2)))

    let points = [P(1,0); P(0,1); P(1,1)]

    let sys =
        points 
        |> List.map (fun p -> {Comp.Create() with I = initLoc p })
        |> Set.ofList
        |> Set.add {Comp.Create() with P = proc; I = initLoc (P(0,0))}
        |> Set.add {Comp.Create() with P = proc2; I = initLoc (P(2, 1)) }

    let mutable s = sys
    let mutable count = 0

    do (print s |> ignore)
    do (print count |> ignore)

    while not (isIdle s) do 
        let newS, lbl = stepLabel s
        do (lbl.Value |> print |> ignore)
        s <-  newS
        s |> Seq.sortBy (fun x -> x._Id) |> List.ofSeq |> print |> ignore
        count <- count + 1 |> print

    0 // return an integer exit code