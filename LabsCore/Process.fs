namespace LabsCore
open Tokens
open Grammar
open FSharpPlus.Lens
open FParsec


/// Functions to manipulate Process objects.
module Process =
    /// General-purpose fold function.
    let rec fold fbase fguard fcomp acc proc =
        let recurse = fold fbase fguard fcomp
        match proc with
        | BaseProcess b ->
            fbase acc b
        | Guard n  ->
            recurse (fguard acc (fst n.Def)) (snd n.Def)
        | Comp(typ, l) -> 
            fcomp typ recurse acc l
    
    /// <summary>Catamorphism function for processes.</summary>
    /// <remark>"A catamorphism is a function that "collapses" a recursive type into
    /// a new value based on its structure. In fact, you can think of a catamorphism
    /// as a sort of "visitor pattern". (S. Wlaschin, "F# for fun and profit")</remark>
    /// <param name="fbase">Function to apply to <c>BaseProcess</c> objects.</param>
    /// <param name="fguard">Function to apply to <c>Guard</c> objects.</param>
    /// <param name="fcomp">Function to apply to <c>Comp</c> objects.</param>
    /// <param name="proc">The input process.</param>
    let rec cata fbase fguard fcomp proc = 
        let recurse = cata fbase fguard fcomp
        match proc with
        | BaseProcess b -> fbase b
        | Guard n ->
            fguard n (recurse (snd n.Def))
        | Comp (typ, l) -> fcomp typ (l |> List.map recurse)

    /// <summary>
    /// Recursively applies <c>fbase</c> and <c>fguard</c> to <c>proc</c>.
    /// </summary>
    let rec map fbase fguard proc =
        let recurse = map fbase fguard
        match proc with
        | BaseProcess b -> fbase b
        | Guard n ->
            let g, p = n.Def
            let p' = recurse p
            Guard(setl _def (fguard g, p') n)
        | Comp(typ, l) -> Comp(typ, List.map recurse l)

    /// Pretty-prints <c>proc</c> to a string.
    let rec print proc =
        let printNode b = string b.Def
        let printGuard g =
            sprintf "%O %s %s" g tGUARD
        let rec printComp typ l = 
            let sep = 
                match typ with 
                | Seq -> $"%s{tSEQ} "
                | Choice -> $" %s{tCHOICE} "
                | Par -> $" %s{tPAR} "
            String.concat sep l
            |> if (Seq.length l) > 1 then (sprintf "(%s)") else id
        cata printNode printGuard printComp proc
    
    /// Simplifies the process by removing <c>Comp</c>
    /// elements with only one child.
    let simplify proc =
        let comp typ l =
            if List.length l = 1 then l.Head
            else Comp(typ, l)
        let guard n p =
            Guard(setl (_def << _2) p n)
        cata BaseProcess guard comp proc
    
    /// Transforms g -> Par(...) into g -> (Skip; Par(...)).
    let fixGuardedPar proc =
        let guard n p = 
            let p' =
                match p with
                | Comp (Par, _) -> Comp(Seq, [BaseProcess({Name=""; Pos=n.Pos; Source=""; Def=Skip}); p])
                | _ -> p
            Guard(setl (_def << _2) p' n)
        cata BaseProcess guard (fun typ l -> Comp(typ, l)) proc

    let private entryOrExit fn =
        let entrycomp = function
        | Seq -> fn
        | Choice | Par -> Set.unionMany
        cata Set.singleton (fun _ -> id) entrycomp

    /// Returns the entry base processes of proc.
    let initial proc = entryOrExit List.head proc

    /// Returns the final base processes of proc.
    let final proc = entryOrExit List.last proc

    /// Adds lbl to the position of a node.
    let tag lbl node =
        let p=node.Pos in let p' = Position(lbl, p.Index, p.Line, p.Column)
        setl _pos p' node
    
    
    /// <summary>
    /// Replaces (non-recursive) <c>Name</c> processes with their definitions.
    /// </summary>
    /// <remark>Inserts information about the <c>Name</c> process into its expansion's elements.</remark>
    /// <param name="procDefs">Process definitions (a <c>Map</c> from <c>string</c> to <c>Process</c>.</param>
    /// <param name="name">The name of the process being expanded.</param>
    let expand (procDefs: Map<_, _>) name =
        let rec expandFn visited name = 
            let baseFn b = 
                match b.Def with
                | Name n when n=name || n="Behavior" -> BaseProcess b
                | Name n when (not (Set.contains b visited)) ->
                    match procDefs.TryFind n with
                    | Some _ -> 
                        expandFn (visited.Add b) n 
                        |> map ((tag $"%s{n}@{b.Pos}") >> BaseProcess) id
                    | None -> failwith n
                | _ -> BaseProcess b
            map baseFn id procDefs.[name]
        expandFn Set.empty name
        
    /// Returns the set of "pick" variables defined in a process p
    /// Notice that p must be already unfolded
    let collectPicks p =
        let doAction a =
            match a.ActionType with
            | Pick _ -> Set.singleton (string a.Updates.Head) 
            | _ -> Set.empty
        
        let fbase acc (b:Node<Stmt<_>>) =
            match b.Def with
            | Act a -> doAction a |> Set.union acc
            | Block b -> List.map doAction b |> Set.unionMany |> Set.union acc
            | Name _
            | Skip _
            | Nil _ -> acc
            
        let fcomp _ recurse acc procs =
            List.map (recurse Set.empty) procs
            |> Set.unionMany
            |> Set.union acc
            
        fold fbase (fun acc _ -> acc) fcomp Set.empty p