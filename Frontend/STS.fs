module Frontend.STS

open LabsCore
open Grammar
open FSharpPlus.Lens

type ExecPoint = Map<int, int>
type EntryCond = Map<int, int>
type ExitCond = Map<int, Set<int>>

type Transition =
    { Entry: EntryCond
      Siblings: Set<int>
      Last: bool
      Exit: ExitCond
      Action: Node<Stmt<Var<int> * int>> }

type TransitionSystem = Set<Transition>

type State =
    { Sts: TransitionSystem
      InitCond: ExitCond }

module internal ExitCond =
    let ofEntryConds s =
        Seq.fold
            (Map.fold (fun (st: Map<_, _>) k v ->
                match st.TryFind k with
                | Some oldset -> Map.add k (Set.add v oldset) st
                | None -> Map.add k (Set.singleton v) st))
            Map.empty
            s

    /// Returns a new exitcond with entry removed from the given exitcond
    let remove (entry: EntryCond) (exit: ExitCond) =
        let _remove key elem table =
            Map.tryFind key table
            |> Option.map (fun s -> Map.add key (Set.remove elem s) table)
            |> Option.defaultValue table

        Map.fold (fun ex pc v -> _remove pc v ex) exit entry

/// Returns the disjunction of two exit conditions.
let (.>>.) e1 e2 =
    Map.unionWith (fun _ -> Set.union) e1 e2

/// Returns the disjunction of two exit conditions.
/// If v[k] terminates in e1 but not in e2, the termination is removed
let (>>.) e1 e2 =
    Map.unionWith (fun _ (s1: Set<_>) (s2: Set<_>) -> Set.union (s1.Remove 0) s2) e1 e2

let (|-) (entry: EntryCond) (exit: ExitCond) =
    Map.toSeq entry
    |> Seq.forall (fun (pc, v) ->
        match exit.TryFind pc with
        | Some s -> Set.contains v s || (Set.contains -1 s && v <> 0)
        | None -> true)

module ExecPoint =
    let empty = [ (0, 0) ] |> Map.ofList

    let freshpc k v =
        match Map.tryFind k v with
        | Some x -> let x' = x + 1 in (Map.add k x' v), x'
        | None -> Map.add k 1 v, 1

    let newpc n v =
        let maxpc = Map.keys v |> Seq.max
        let newpcs = Set.ofSeq { maxpc + 1 .. maxpc + n }

        newpcs
        |> Set.map (fun n -> n, 0)
        |> Map.ofSeq
        |> fun v' -> Map.union v v', newpcs

let rec removeNames (procs: Map<string, Process<_>>) lts =
    let rec newexit name (pos: FParsec.Position) =
        let inits =
            (*Find initial actions and tag them*)
            let initials =
                Process.initial procs.[name]
                |> Set.map (
                    if name = "Behavior" then
                        id
                    else
                        fun x ->
                            match x.Def with
                            | Name _ -> x
                            | _ -> (Process.tag pos.StreamName) x
                )

            let nestedNames =
                initials
                |> Set.map (fun x ->
                    match x.Def with
                    | Name s -> Some(s, x.Pos)
                    | _ -> None)
                |> Seq.choose id
                |> Set.ofSeq

            if nestedNames.IsEmpty then
                initials, Set.empty
            else
                let nonNames =
                    Set.filter
                        (fun x ->
                            match x.Def with
                            | Name _ -> false
                            | _ -> true)
                        initials

                nonNames, nestedNames

        let actions, otherNames = inits
        let recurseOnNames = Set.map (fun (n, pos) -> newexit n pos) otherNames

        let recurseEntry =
            if recurseOnNames.IsEmpty then
                Map.empty
            else
                Seq.reduce (.>>.) recurseOnNames

        lts
        |> Set.filter (fun tr ->
            actions.Contains tr.Action
            || (Set.exists (fun i -> i.Def = tr.Action.Def) actions
                && tr.Action.Pos.StreamName.Contains($"{name}@{pos}")))
        |> Set.map (fun t -> t.Entry)
        |> ExitCond.ofEntryConds
        .>>. recurseEntry


    let removeName (l: TransitionSystem) (tr: Transition) =
        let exit =
            match tr.Action.Def with
            | Name s -> newexit s tr.Action.Pos
            | _ -> failwith "Something wrong happened"

        let affected, others =
            l
            |> Set.remove tr
            |> Set.partition (fun t -> tr.Entry |- t.Exit && (t.Siblings.IsEmpty || t.Last))

        affected
        |> Set.map (fun t -> { t with Exit = (ExitCond.remove tr.Entry t.Exit) >>. exit })
        |> Set.union others

    lts
    |> Set.filter (fun t ->
        match t.Action.Def with
        | Name _ -> true
        | _ -> false)
    |> Seq.fold removeName lts


/// Helper type for function makeTransitions
type private Accumulator = TransitionSystem * (int * EntryCond * EntryCond * ExitCond)
// This is the implementation of the [[ P ]] function in our latest paper.
// Most of the state is passed around in the `acc` tuple.
// It returns a set of transitions and the initial values for the exec. point.
let makeTransitions (state: Accumulator) proc =
    let mutable (initCond: ExitCond) = Map.empty
    let initials = Process.initial proc

    let baseFn (lts, acc) b =
        let k, v, parent, exit = acc
        let v', vk = ExecPoint.freshpc k v
        let entry = (Map.add k vk parent)
        (* If b is an initial action of procs, add its entry condition to initCond *)
        initCond <-
            initCond
            .>>. if initials.Contains b then
                     ExitCond.ofEntryConds [ entry ]
                 else
                     Map.empty

        let lts' =
            (Set.add
                { Entry = entry
                  Exit = exit
                  Action = b
                  Siblings = Set.empty
                  Last = false })
                lts

        lts', (setl _2 v' acc)

    let rec compFn typ recurse (lts, acc) l =
        match typ with
        | Seq ->
            let p = List.last l
            let lts', acc' = recurse (Set.empty, acc) p

            let exit': Map<int, Set<int>> =
                Set.filter (fun t -> (Process.initial p).Contains t.Action) lts'
                |> Set.map (fun t -> t.Entry)
                |> ExitCond.ofEntryConds

            let acc'' = setl _4 exit' acc'

            match l with
            | []
            | [ _ ] -> (Set.union lts lts', acc'')
            (* recurse on l without its last element*)
            | _ -> compFn Seq recurse (Set.union lts lts', acc'') ((List.rev << List.tail << List.rev) l)
        | Choice ->
            (* Recurse on l, accumulate the lts and the exec point but reset the exit *)
            List.fold (fun s -> recurse (setl (_2 << _4) (acc ^. _4) s)) (lts, acc) l
        | Par ->
            let k, v, _, exit = acc
            let v', fresh = ExecPoint.freshpc k v
            let v'', newpcs = ExecPoint.newpc l.Length v'

            let childRecurse (l, a) pc child =
                let siblings = Set.filter ((<>) pc) newpcs
                let a' = (setl _1 pc >> setl _4 ([ (pc, Set.singleton 0) ] |> Map.ofList)) a
                let l', a'' = recurse (Set.empty, a') child

                let last, others =
                    Set.partition (fun t -> (Process.final child).Contains t.Action) l'

                Set.map
                    (fun t ->
                        let t' = { t with Siblings = siblings }

                        Set.singleton t'
                        |> Set.add
                            { t' with
                                Last = true
                                Exit = t'.Exit .>>. exit })
                    last
                |> Set.unionMany
                |> Set.union others
                |> fun x -> Set.union l x, a''

            (* update execpoint and parent *)
            let acc' = (setl _2 v'' >> over _3 (Map.add k fresh)) acc

            let children = Seq.zip newpcs l

            Seq.fold (fun (l, a) (k', p) -> childRecurse (l, a) k' p) (Set.empty, acc') children
            |> setl (_2 << _1) k

    (* No matter what exit is passed to the function, the base exitcond is always pc[k]==0 *)
    let (exit: Map<int, Set<int>>) =
        [ ((view (_2 << _1) state), Set.singleton 0) ] |> Map.ofList

    let state' = setl (_2 << _4) exit state
    Process.fold baseFn (fun i _ -> i) compFn state' proc, initCond
