namespace Buzz
open Buzz.Functions
open Buzz.Component

    module System = 
        /// A system is a set of components
        type Sys = Set<Comp>

        type Condition =
            | TT
            | FF
            | KeyConsensus of Key list
            | AtStep of int
            | AfterStep of int
            member this.HoldsFor (sys:Sys) (step) =
                match this with
                | TT -> true
                | FF -> false
                | KeyConsensus(keys) ->
                    let agreeOn (c1:Comp) (c2:Comp) k =
                        match (c1.L.[k], c2.L.[k]) with
                        | Some(p1), Some(p2) -> (fst p1) = (fst p2)
                        | _ -> false

                    let min = sys.MinimumElement
                    let v = Set.forall (fun c -> List.forall(agreeOn c min) keys) sys
                    v
                | AtStep(n) -> step = n
                | AfterStep(n) -> step > n

        /// <summary>Events are changes in the system that the programmer can
        /// use to simulate a dynamic environment.</summary>
        /// An event is a Condition * (Comp -> Comp) tuple.
        /// When the system meets the condition, the function is applied to all
        /// its components.
        type Event = Condition * (Comp -> Comp)

        type TraceStep = int * Sys * Label


        let confirmTransitions (c:Comp) = 
            c.EntriesQ()
            |> Set.map (fun entry -> 
                (c, Qry(c.I, entry), {c with _StackQ = c._StackQ.Remove (fst entry)}))
        
        let propagateTransitions (c:Comp) = 
            c.EntriesP()
            |> Set.map (fun entry -> 
                (c, Put(c.I, entry), {c with _StackP = c._StackP.Remove (fst entry)}))

        /// Return all available transitions for `sys`
        let transitions (sys: Sys) = 
            sys 
            |> Set.map (fun c -> c.Transitions())
            |> Set.map Set.ofList
            |> Set.union (Set.map propagateTransitions sys)
            |> Set.union (Set.map confirmTransitions sys)
            |> Set.unionMany

        /// <summary>Apply a transition to system <c>sys</c>.</summary>
        /// <remarks>Implements the semantics of a Buzz system.</remarks>
        /// <param name="sys">The system before the transition.</param>
        /// <param name="(cmp, lbl, nextCmp)">The transition to apply.</param>
        /// <returns>The system after performing the transition and become
        /// <c>s</c>.</returns>
        let apply link (sys :Sys) (cmp, lbl, nextCmp) =
            let isValid = Set.contains cmp sys
            if not isValid then failwith "Incorrect transition" else
                let newSys = sys.Remove(cmp)
                match lbl with
                //| Write(l, pair) -> newSys
                | Put(i, (k, (v, t))) ->
                    newSys
                    |> Set.map (fun c ->
                        if (link i c.I) then
                            {c with 
                                L = c.L + (k, (v,t));
                                _StackP = c._StackP.Add k;
                                _StackQ = c._StackQ.Remove k;
                            }
                         else c)
                    |> Set.add nextCmp
                | Qry(i, (k, (v, t))) ->
                    let (neighbors, others) = Set.partition (fun c -> link i c.I) newSys
                    neighbors
                    |> Set.map (fun c ->
                        // olderValue = true if L(k)=nil or time(L, k) < t
                        let olderValue = 
                            c.L.TimeOf k
                            |> Option.forall ((>=) t)
                        if olderValue then
                            // Rule QRY1 
                            {c with 
                                L = c.L + (k, (v,t));
                                _StackP = c._StackP.Add k;
                                _StackQ = c._StackQ.Remove k;
                            }
                        else
                            // Rule QRY2
                            {c with 
                                _StackP = c._StackP.Add k;
                            }
                    )
                    |> Set.union others
                    |> Set.add nextCmp
                | Eps -> newSys.Add(nextCmp)

        /// Returns true if sys cannot perform any transition.
        let isIdle (sys:Sys) =
            transitions sys |> Seq.isEmpty

        /// Use function lfunc to re-project the locations of component c.
        let reproject lfunc c =
            let loc = c.I.["loc"] 
            let newLoc = 
                match loc with 
                | P(p) -> P(lfunc p) 
                | _ -> failwith "Error"
            {c with I = c.I.Add("loc", newLoc)}

        /// <summary>
        /// Returns the evolution of <c>sys</c> after performing a random transition.
        /// If no transition is available, returns <c>sys</c>.
        /// </summary>
        let step (sys: Sys) link = 
            if (isIdle sys) then sys
            else
            sys
            |> transitions
            |> pickRandom
            |> Option.get
            |> apply link sys 

        /// Performs a random transition and returns the new system,
        /// along with its label
        let stepLabel link sys =
            if (isIdle sys) then None
            else
                let t = sys |> transitions |> pickRandom |> Option.get
                let _, lbl, _ = t
                Some(apply link sys t, lbl)
                                 
        let guidCheck sys =
            let ids = sys |> Set.map (fun c -> c._Id)
            ids.Count = sys.Count
        
        ///<summary>
        /// Returns a trace of <c>sys</c> using <c>link</c> as the connectivity function.
        /// The trace ends when no transitions are available or the break condition
        /// <c>cond</c> is satisfied.
        /// Notice that the sequence might be infinite.
        ///</summary>
        let rec run sys link (breakCond:Condition) (events: Event list) =
            let applyEvent count sys ((c,f):Event) =
                (if c.HoldsFor sys count then Set.map f else id) sys

            let rec traceof sys count = seq {
                if (breakCond.HoldsFor sys count) then ()
                else
                    match stepLabel link sys with
                    | Some(newSys, lbl) ->
                        let newSysWithEvents = 
                            events 
                            |> List.fold (applyEvent count) newSys
                        yield (count, newSysWithEvents, lbl)
                        yield! traceof newSysWithEvents (count+1)
                    | None -> ()
            }
            traceof sys 1