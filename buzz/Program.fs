// Learn more about F# at http://fsharp.org

open System
open Buzz.Types
open Buzz.Functions
open Buzz.Component


/// A system is a set of components
type Sys = Set<Comp>

let rng = Random()

/// Helper: choose random element from a Seq
let pickRandom seq =
    Seq.tryItem ((Seq.length >> rng.Next) seq) seq

/// Return all available transitions for `sys`
let transitions (sys: Sys) = 
    sys 
    |> Set.map (fun c -> c.Transitions())
    |> Set.map Set.ofList
    |> Set.unionMany

/// <summary>Apply a transition to system <c>sys</c>.</summary>
/// <remarks>Implements the semantics of a Buzz system.</remarks>
/// <param name="sys">The system before the transition.</param>
/// <param name="(cmp, lbl, nextCmp)">The transition to apply.</param>
/// <returns>The system after performing the transition and become
/// <c>s</c>.</returns>
let apply (sys :Sys) (cmp, lbl, nextCmp) =
    let isValid = Set.contains cmp sys
    if not isValid then failwith "Incorrect transition" else
        let newSys = sys.Remove(cmp)
        match lbl with
        | Write(l, pair) ->
            newSys
            |> Set.map( 
                fun c -> 
                if link(l, c.I.["loc"])
                then {c with P = Put(pair) ^. c.P}
                else c)
            |> Set.add nextCmp
        | Read(l, pair) ->
            newSys
            |> Set.map(
                fun c ->
                    if link(l, c.I.["loc"])
                    then 
                        if c.K.Accepts pair
                        then {c with P = Put(pair) ^. c.P}
                        else 
                            let cPair = c.K.TpairOf(fst pair).Value
                            if not (fst (snd cPair) = fst (snd pair))
                            then {c with P = Send(cPair) ^. c.P}
                            else c
                    else c
            )
            |> Set.add nextCmp
        | Eps -> newSys.Add(nextCmp)


/// Returns true if sys cannot perform any transition.
let isIdle (sys:Sys) =
    transitions sys |> Seq.isEmpty

/// <summary>
/// Returns the evolution of <c>sys</c> after performing a random transition.
/// If no transition is available, returns <c>sys</c>.
/// </summary>
let step(sys: Sys) = 
    if (isIdle sys) then sys
    else
    sys
    |> transitions
    |> pickRandom
    |> Option.get
    |> apply sys

let stepLabel sys =
    if (isIdle sys) then (sys, None)
    else
        let t = sys |> transitions |> pickRandom |> Option.get
        let _, lbl, _ = t
        (apply sys t, Some(lbl))
     

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