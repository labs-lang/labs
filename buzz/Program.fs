// Learn more about F# at http://fsharp.org

open System

type Point = int * int

let d (p1: Point, p2: Point) =
    (float ((fst p1) - (fst p2)))**2.0 + (float ((snd p1) - (snd p2)))**2.0
    |> sqrt;;

type AttrVal =
    | Int of int
    | String of string
    | P of Point

type Val = int
type Tval = Val * DateTime
let timeof ( tv: Tval ) = snd tv

type Tkey = string
type Tpair = Tkey * Tval

type Interface = Map<string, AttrVal>

let initLoc p = Map.empty<string, AttrVal>.Add("loc", p)


/// A timestamped map. Each key has one pair (value, timestamp)
type Tmap = 
    private { d : Map<Tkey, Tval> }
   
    static member Empty = 
        {Tmap.d = Map.empty<Tkey, Tval>}
    
    static member (!=) (left: Tmap, right: Tmap) = 
        not (left.d = right.d)

    static member (+) (left: Tmap, right: Tpair) = 
        let isNewer = 
            right
            |> fst
            |> left.d.TryFind 
            |> Option.forall (fun v -> timeof (snd right) > timeof v)
        if isNewer then {left with d = left.d.Add right } else left

    member this.Item
        with get(k) = this.d.TryFind(k)

type Action =
    | Attr of string * AttrVal
    | Put of Tpair
    | Await of Tkey * Val
    override this.ToString() = 
        match this with
        | Attr(a, v) -> sprintf "[%s := %s]" a (v.ToString())
        | Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
        | Await(k, v) -> sprintf "<%A = %A>" k v

[<StructuredFormatDisplay("{AsString}")>]
type Process = 
    | Nil
    | Proxy of string * Lazy<Process>
    | Prefix of Action * Process
    static member ( ^. )(left: Action, right: Process) =
        Prefix(left, right)
    member this.AsString = this.ToString()        
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Proxy(name, p) -> name 
        | Prefix(action, proc) -> sprintf "%s.%s" (action.ToString()) proc.AsString


#nowarn "0342"
type Comp = 
    private { K: Tmap; I : Interface; P : Process }

    interface IComparable with
        member x.CompareTo y =
            match y with
            | :? Comp as yComp -> hash(x).CompareTo(hash(yComp))
            | _ -> 1
        
    //override x.Equals y = 
        //match y with
        //| :? Comp as yComp -> not (x <> yComp)
        //| _ -> false

    member this.Distance(other: Comp) =
        let myLoc = this.I.["loc"]
        let hisLoc = other.I.["loc"]
        match (myLoc, hisLoc) with
        | (Int(a), Int(b)) -> abs (a - b) |> float
        | (String(a), String(b)) -> infinity
        | (P(p1), P(p2)) -> d(p1, p2)
        | _ -> infinity
    
    member this.IsIdle() =
        match this.P with
        | Nil -> true
        | _ -> false

    member this.Transitions() =
        match this.P with
        | Proxy(_, proc) -> {this with P = proc.Value }.Transitions()
        | Nil -> []
        | Prefix(action, proc) -> 
            match action with
            | Attr(a, v) -> [(this, action, {this with I=this.I.Add(a, v); P=proc})]
            | Put(pair) -> [(this, action, {this with K=this.K + pair; P=proc})]
            | Await(k, v) -> 
                // The awareness operator is atomic
                if this.Check(k,v) then 
                    {this with P = proc}.Transitions()
                    |> List.map (fun (c, a, nc) -> (this, a, nc))
                else []
    
    member this.Check(k: Tkey, v: Val) =
        this.K.[k]
        |> Option.exists (fun p -> v.Equals(fst p))

and CompTransition = Comp * Action * Comp

/// A system is a set of components
type Sys = Set<Comp>

let rng = Random()
let DELTA = 1.0

/// Helper: choose random element from a Seq
let pickRandom seq =
    Seq.tryItem ((Seq.length >> rng.Next) seq) seq

/// Return all available transitions for `sys`
let transitions (sys: Sys) = 
    sys 
    |> Set.map (fun c -> c.Transitions())
    |> Set.map Set.ofList
    |> Set.unionMany

/// Apply a transition to system `sys`. Return Some(s) if sys can perform the
/// given transition and become `s`, otherwise None
let apply sys ((cmp, action, nextCmp):CompTransition) =
    let isValid = transitions sys |> Set.contains (cmp, action, nextCmp)
    if not isValid then None else
        let newSys = sys.Remove(cmp)
        match action with
        | Put(p) when cmp.K != nextCmp.K -> 
            newSys
            |> Set.map (fun c -> 
                if cmp.Distance(c) <= DELTA
                then match c.P with
                     | Prefix(a, _) when a = action -> c
                     | _ -> {c with P= action ^. c.P }
                else c)
            |> Set.add (nextCmp)
            |> Some
        | _ -> Some(newSys.Add(nextCmp))

/// Return the evolution of `sys` after performing a random transition.
/// If no transition is available, return `sys`
let step(sys: Sys) = 
    let t = (pickRandom << transitions) sys
    t |> (Option.bind << apply) sys
    |> Option.defaultValue sys

/// Returns true if sys cannot perform any transition
let isIdle (sys:Sys) =
    transitions sys |> Seq.isEmpty

let print x =
    printfn "----\n%A" (x)
    x

/// Return a Put action with a "fresh" timestamp
let PutNow (k:Tkey, v:Val) =
    Put((k, (v, DateTime.Now)))


[<EntryPoint>]
let main argv = 
    printfn "Hello World from F#!"

    // Some basic processes
    //let proc = PutNow("x", 1) ^. Nil
    //let proc2 = Await("x", 1) ^. PutNow("x", 2) ^. Nil

    // We can also have recursive process definitions,
    // but we must define them as unit -> Process functions
    // to work well with F# type inference
    let rec prova ():Process = PutNow("x", 1) ^. Await("x", 2) ^. Proxy("Pr1", lazy (prova()))
    let rec prova2 ():Process = Await("x", 1) ^. PutNow("x", 2) ^. Proxy("Pr2", lazy (prova2()))



    // Stub component
    let comp = {
        Comp.K = Tmap.Empty; 
        I = initLoc (P(0,0));
        P = Nil
    }

    let points = [P(1,0); P(0,1); P(1,1)]

    let sys =
        {comp with P = prova()}  ::
        (
        points
        |> List.map (fun p -> {comp with I = initLoc p })
        )
        |> Set.ofList
    
    let mutable s = sys.Add {comp with P = prova2(); I = initLoc (P(2, 1)) }
    let mutable count = 0

    do (print s |> ignore)
    do (print count |> ignore)

    while not (isIdle s) do 
        s <-  step s
        s |> Seq.sortBy (fun x -> x.K.["loc"]) |> List.ofSeq |> print |> ignore
        count <- count + 1 |> print
        // printfn "%A" (transitions s)

    0 // return an integer exit code