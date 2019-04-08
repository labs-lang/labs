module internal Encode
open LabsCore
open Types
open Link
open Base
open Templates
open Expressions
open Properties
open Liquid
open FSharpPlus.Operators
open FSharpPlus.Lens
open Tokens

/// Computes a unique entry point for each base process in the input process.
let setentry info =
    let base_ (info: EntryPointInfo<_>) b =
        match b.stmt with
        | Name _ -> info (*do not assign an entrypoint to name *)
        | _ ->
            let newinfo = info.increment
            {newinfo with
                entrypoints = info.entrypoints.Add (b, (newinfo.pcs, newinfo.par, newinfo.mypc))
            }
    let comp_ typ recurse (acc:EntryPointInfo<_>) l =
        match typ with
        | Seq
        | Choice -> Seq.fold recurse acc l
        | Par -> 
            match l with
            | [] -> acc
            | p :: [] -> recurse acc p
            | _ ->
                // First, add 1 to the current pc
                let increment = acc.increment
                // Explore one subprocess at a time.
                let folded =
                    // This is just the set of children pcs that will be spawned
                    let par = set [ increment.mypc + 1 .. increment.mypc + l.Length ]
                    List.fold (fun i subp ->
                    let resetpc = {i with pcs=increment.pcs; par=par} // Each time, start again from increment.pcs
                    recurse (resetpc.spawnpc 1) subp) increment l // Give a different pc to each subprocess
                // Continue with the parent pc
                {increment with entrypoints=folded.entrypoints}
    Process.fold base_ (fun a _ -> a) comp_ info

/// Computes the guards for each base process in proc.
let setGuards proc =
    let base_ (guards, acc) b = (guards, Map.add b guards acc)
    let guard_ (guards, acc) g = (Set.add g guards, acc)
    let comp_ typ recurse (guards, acc) (l:List<_>) =
        if l.IsEmpty then (guards, acc) else
        match typ with
        | Seq ->
            // Guards only affect the first process in a sequence
            let (_, ha) = recurse (guards, acc) l.Head
            recurse (Set.empty, ha) (Comp(Seq, l.Tail)) 
        | _ ->
            Seq.map (recurse (guards, acc)) l
            |> Seq.reduce(fun (_, a1) (g2, a2) -> g2, Map.union a2 a1)
    Process.fold base_ guard_ comp_ (Set.empty, Map.empty) proc                    
    |> snd

/// Computes an exit point for all (non-terminal) base process in proc.
let setExit (entrypoints:EntryPoint<_>) (procs:Map<_,_>) proc =
    let e' b = entrypoints.[b]^._1
    
    let joinEntrypoints s =
        Seq.fold (
            Map.fold (fun (st:Map<_,_>) k v ->
                match st.TryFind k with
                | Some oldset -> Map.add k (Set.add v oldset) st
                | None -> Map.add k (Set.singleton v) st
            )    
        ) Map.empty s
    let toExits entrypoint = joinEntrypoints (Seq.singleton entrypoint)
    
    let rec setExit_ (curExit, acc) = function
    | BaseProcess b ->
        let findRecursion name (pos:FParsec.Position) =
            Process.entry procs.[name]
            |> Set.map (if name = "Behavior" then id else chStreamName pos.StreamName)
            |> Set.map e' 
            |> joinEntrypoints

        let exits =
            match b.stmt with
            | Name n -> findRecursion n b.pos
            | Nil //-> Map.empty //todo
            | _ -> e' b |> toExits
        (exits, Map.add b curExit acc)
    | Guard(_,p,_) -> setExit_ (curExit, acc) p
    | Comp(Seq, l) ->
        match l with
        | [] -> (curExit, acc)
        | hd::[] -> setExit_ (curExit, acc) hd
        | hd::tl ->
            let acc' = setExit_ (curExit, acc) (Comp(Seq, tl))
            setExit_ acc' hd
    | Comp(_, l) ->
        List.map (setExit_ (curExit, acc)) l
        |> List.reduce (fun (e1,a1) (e2,a2) -> Map.unionWith (fun _ -> Set.union) e1 e2, Map.union a1 a2)
    setExit_ (Map.empty, Map.empty) proc
    |> snd

/// Returns a C encoding for a process
let encode sync (entry:EntryPoint<_>) (exit:Map<_,_>) (guards:Map<_,_>) =
    let base_ (b:Base<_,_>) =
        let action = match b.stmt with | Act a -> Some a | _ -> None
        
        /// Set of keys that the agent will have to confirm
        let qrykeys =
            let getLstigVarsBExpr =
                let compare_ _ e1 e2 = Set.union (getLstigVars e1) (getLstigVars e2)
                BExpr.cata (fun _ -> Set.empty) id compare_ (fun _ -> Set.union)
            action
            |>> (fun a -> List.map (getLstigVars << snd) a.updates)
            |>> Set.unionMany
            |> Option.orElse (Some Set.empty)
            |>> Set.union (guards.[b] |> Set.map getLstigVarsBExpr |> Set.unionMany)
            |>> Seq.map (Int << snd)
            |> Option.defaultValue (Seq.empty)
            |> Lst
        
        let liquidAssignment (k:Ref<Var*int, unit>, expr) = 
            let size = match (fst k.var).vartype with Array s -> s | _ -> 0
            Dict [
                "key",  Int (snd k.var)
                "offset",
                    k.offset |>> (procExpr.ExprTranslator >> Str) |> Option.defaultValue (Int 0)
                "size", Int size
                "expr", procExpr.ExprTranslator expr |> Str
            ]
            
        let mypc = entry.[b]^._3
        let exits =    
            (* if there is no value for mypc, then this is a terminal process *)
            if not <| Map.containsKey mypc exit.[b] then 
                exit.[b].Add (mypc, (Set.singleton 0))
            else exit.[b]
        
        let parPcs =
            (*add check on parallel pcs only to terminal processes*)
            if not <| Map.containsKey mypc exit.[b] then
                seq (entry.[b]^._2) 
            else Seq.empty
            
        let mypcExit, otherExits = Map.partition (fun k _ -> k = mypc) exits            
            
        [
            "label", funcName (entry.[b]^._1) |> Str
            "entrypoints", liquidPcs (entry.[b]^._1 |> Map.mapValues Set.singleton) |> Lst
            "mypcexit", liquidPcs mypcExit |> Lst
            "otherexits", liquidPcs otherExits |> Lst
            "parcheck", parPcs |> Seq.map Int |> Lst
            "guards",
                guards.[b]
                |> Set.map (procExpr.BExprTranslator)
                |> Seq.map Str
                |> Lst
                
            "labs",
                (action |>> string |> Option.defaultValue "Skip")
                |> (+) tGUARD
                |> (+) (guards.[b] |> Set.map string |> String.concat " and ")
                |> Str
            "type", (action
                |>> fun a -> a.actionType
                |>> function | I -> "attr" | L _ -> "lstig" | E -> "env"
                |> Option.defaultValue ""
                |> Str)
            "qrykeys", qrykeys
            "sync", sync |> Bool
            "assignments", (action
                |>> fun a -> a.updates
                |>> Seq.map liquidAssignment
                |> Option.defaultValue Seq.empty
                |> Lst
            )                
        ]
        |> fun x -> (strRender x) goto
        |> Result.mapError failwith
    
    let comp_ (r1:Result<_,_>) (r2:Result<_,_>) =
        r1 >>= (fun x -> (r2 |>> (sprintf "%s\n%s" x)))
    
    Process.cata
        (fun b -> match b.stmt with | Name _ -> Ok "" | _ -> base_ b)
        (fun _ _ -> id) (fun _ -> List.reduce comp_)

         
let translateHeader isSimulation noBitvectors bound sys (mapping:KeyMapping) (entrypoints: EntryPoint<_>) (maxI, maxL, maxE) =
    let getTypedef num = 
        let getStandardTypes = 
            function
            | a, b when a >= 0     && b < 256      -> "unsigned char"
            | a, b when a > -128   && b < 128      -> "char"
            | a, b when a >= 0     && b < 65536    -> "unsigned short"
            | a, b when a > -32768 && b < 32768    -> "short"
            | a, _ when a >= 0                     -> "unsigned int"
            | _ -> "int "
        let bitwidth num = 
            System.Math.Log(float num, 2.) |> int |> (+) 1
        if noBitvectors
        then getStandardTypes (0, num)
        else sprintf "unsigned __CPROVER_bitvector[%i]" (bitwidth num)
    
    let (tupleStart, tupleEnd), maxTuple =
        
        /// Finds the min and max indexes of the given tuple.
        let extrema (tup:Set<Var>) =
            let indexes =
               Set.map (fun (v:Var) -> mapping.[v.name]) tup
            let endIndexes = 
                tup
                |> Set.map (fun v ->
                    match v.vartype with
                    | Scalar -> 0
                    | Array len -> len - 1 
                    |> (+) mapping.[v.name])

            (Seq.min indexes, Seq.max endIndexes)
        let makeTuple (tup: Set<Var>) =
            let min, max = extrema tup
            List.replicate (max-min+1) (min, max), max-min

        if sys.stigmergies.IsEmpty then ([0], [0]), 0
        else
        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.collect (List.map (makeTuple))
        |> Seq.reduce (fun (l1,s1) (l2,s2) -> List.append l1 l2, max s1 s2)
        |> fun (lst, size) -> List.unzip lst, size+1

    // Find the number of PCs used in the program
    let maxPc =
        entrypoints
        |> Map.values
        |> Seq.map (view _3)
        |> Seq.max

    let maxcomps = 
        Map.fold (fun state _ (_, cmax) -> max state cmax) 0 sys.spawn

    let defines = 
        [
            "BOUND", bound; 
            "MAXCOMPONENTS", maxcomps;
            "MAXPC", maxPc + 1;
            "MAXKEYI", maxI
            "MAXKEYL", maxL
            "MAXKEYE", maxE
            "MAXTUPLE", maxTuple
        ]
        |> (if isSimulation then fun l -> ("SIMULATION", 1)::l else id)
        |> List.map (fun (a, b) -> Dict ["name", Str a; "value", Int b])

    let typedefs =
        [
            "TYPEOFVALUES", "short"
            "TYPEOFPC", "unsigned char"
            "TYPEOFTIME", "unsigned char" 
            "TYPEOFAGENTID", getTypedef maxcomps
            "TYPEOFKEYIID", getTypedef maxI
            "TYPEOFKEYLID", getTypedef maxL
            "TYPEOFKEYEID", getTypedef maxE
            "Bool", getTypedef 1
        ]
        |> List.map (fun (a,b) -> Dict ["name", Str a; "value", Str b])

    let links =
        let makeLink (s:Stigmergy<Var*int>) = 
            let names =
                s.vars
                |> Set.unionMany
                |> Set.map (fun v -> v.name)
            let m = mapping |> Map.filter (fun k _ -> names.Contains k)
            if m.IsEmpty then None else
            Dict [
                "start", Int (Map.values m |> Seq.min)
                "end", Int (Map.values m |> Seq.max)
                "link", s.link |> linkExpr.BExprTranslator |> Str
            ] |> Some

        sys.stigmergies
        |> Map.values
        |> Seq.choose makeLink

    [
        "defines", Lst defines
        "typedefs", Lst typedefs
        "links", Lst links
        "tupleStart", tupleStart |> Seq.map (Str << string) |> Lst
        "tupleEnd", tupleEnd |> Seq.map (Str << string) |> Lst
    ]
    |> renderFile "templates/header.c"

let translateMain fair (sys:SystemDef<Var*int>) entrypoints guards =
    let liquidGuards b = 
        Map.tryFind b guards
        |>> Seq.map ((customProcExpr "firstAgent").BExprTranslator)
        |>> Seq.map Str
        |> Option.defaultValue Seq.empty
        
    let finallyP, alwaysP =
        sys.properties
        |> Map.partition (
            fun _ {modality=m} -> 
                match m with Finally -> true | Always -> false)
    let translateProperties props =
        props
        |> Map.mapValues (translateProp sys)
        |> Map.values
        |> String.concat "\n"

    let scheduleElement b (entry, _, _) =
        Dict [
            "name", funcName entry |> Str
            "entry", liquidPcs (entry |> Map.mapValues Set.singleton) |> Lst
            "guards", liquidGuards b |> Lst
        ]
    [
        "firstagent", if sys.spawn.Count = 1 then Int 0 else Int -1
        "fair", Bool fair;
        "schedule", entrypoints |> Map.map scheduleElement |> Map.values |> Lst
        "alwaysasserts",
            alwaysP
            |> translateProperties
            |> indent 4 |> Str
        "finallyasserts",  
            finallyP
            |> translateProperties
            |> indent 4 |> Str
    ]
    |> renderFile "templates/main.c"
