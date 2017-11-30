namespace Buzz
open Buzz.Functions
open Buzz.Component

    module System = 
        /// A system is a set of components
        type Sys = Set<Comp>

        type Condition =
            | FF
            | KeyConsensus of Key list
            | NumberOfSteps of int
            member this.HoldsFor (sys:Sys) (step) =
                match this with
                | FF -> false
                | KeyConsensus(keys) ->
                    let agreeOn (c1:Comp) (c2:Comp) k =
                        match (c1.L.[k], c2.L.[k]) with
                        | Some(p1), Some(p2) -> (fst p1) = (fst p2)
                        | _ -> false

                    let min = sys.MinimumElement
                    let v = Set.forall (fun c -> List.forall(agreeOn c min) keys) sys
                    v
                | NumberOfSteps(n) -> step = n

        /// Events are changes in the system that the programmer can use
        /// to simulate a dynamic environment.
        type Event = 
            | SetAttr of Key * Val
            member this.ApplyTo (sys:Sys) = 
                match this with
                | SetAttr(k,v) -> 
                    sys
                    |> Set.map (fun c -> {c with I = c.I.Add(k,v)})

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
                        if (link l c.I.["loc"])
                        then {c with P = Put(pair) ^. c.P}
                        else c)
                    |> Set.add nextCmp
                | Read(l, pair) ->
                    let (neighbors, others) =
                        newSys
                        |> Set.partition (fun c -> link l c.I.["loc"])  
                    let (accepters, rejectors) =
                        neighbors
                        |> Set.partition (fun c -> c.L.Accepts pair)
                    
                    accepters
                    |> Set.map (fun c -> {c with P = Put(pair) ^. c.P})
                    |> Set.union 
                        (rejectors
                        |> Set.map (fun c -> 
                            let cPair = c.L.TpairOf(fst pair).Value
                            if not (fst (snd cPair) = fst (snd pair))
                            then {c with P = Send(cPair) ^. c.P}
                            else c))
                    |> Set.union others
                    |> Set.add nextCmp
                | Eps -> newSys.Add(nextCmp)

        /// Returns true if sys cannot perform any transition.
        let isIdle (sys:Sys) =
            transitions sys |> Seq.isEmpty

        /// Use function lfunc to re-project the locations of components
        let project lfunc sys =
            sys
            |> Set.map (fun c -> 
                let loc = c.I.["loc"] 
                let newLoc = 
                    match loc with 
                    | P(p) -> P(lfunc p) 
                    | _ -> failwith "Error"
                {c with I = c.I.Add("loc", newLoc)})

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
            |> project (torusProj 10 10)

        /// Performs a random transition and returns the new systen,
        /// along with its label
        let stepLabel sys =
            if (isIdle sys) then None
            else
                let t = sys |> transitions |> pickRandom |> Option.get
                let _, lbl, _ = t
                Some(apply sys t |> project (torusProj 10 10), lbl)
                                 
        let guidCheck sys =
            let ids = sys |> Set.map (fun c -> c._Id)
            ids.Count = sys.Count

        ///<summary>
        /// Returns a trace of <c>sys</c> as a sequence of <c>Sys * Label</c> pairs.
        /// The trace ends when no transitions are available or the break condition
        /// <c>cond</c> is satisfied.
        /// Notice that the sequence might be infinite.
        ///</summary>
        let rec run sys (breakCond:Condition) =
            let rec s sys count = seq {
                if (breakCond.HoldsFor sys count) then ()
                else
                    match stepLabel(sys) with
                    | Some(newSys, lbl) ->
                        yield (count, newSys, lbl)
                        yield! s newSys (count+1)
                    | None -> ()
            }
            s sys 1