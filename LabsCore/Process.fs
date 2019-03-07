namespace LabsCore
open Tokens
open Types

type Stmt<'a> = 
        | Nil 
        | Skip
        | Act of 'a Action
        | Name of string

type Base<'a, 'b> =
    { stmt: Stmt<'a>; pos: 'b }

type Composition =
        | Seq
        | Choice
        | Par

type Process<'a, 'b> =
    | BaseProcess of Base<'a, 'b>
    | Guard of BExpr<'a, unit> * Process<'a, 'b> * 'b
    | Comp of Composition * Process<'a, 'b> list

module Process =
    let rec fold fbase fguard fcomp acc proc =
        let recurse = fold fbase fguard fcomp
        match proc with
        | BaseProcess b ->
            fbase acc b
        | Guard(g, p, _) ->
            recurse (fguard acc g) p
        | Comp(typ, l) -> 
            fcomp typ recurse acc l
    
    let rec cata fbase fguard fcomp proc = 
        let recurse = cata fbase fguard fcomp
        match proc with
        | BaseProcess b -> fbase b
        | Guard(g, p, pos) -> fguard g pos (recurse p)
        | Comp(typ, l) -> fcomp typ (l |> List.map recurse)

    let rec map fbase fguard proc =
        let recurse = map fbase fguard
        match proc with
        | BaseProcess b -> fbase b
        | Guard(g, p, pos) -> Guard(fguard g, (recurse p), pos)
        | Comp(typ, l) -> Comp(typ, List.map recurse l)

    let rec print proc =
        let print_ b =
            match b.stmt with
            | Nil -> "0"
            | Skip -> "√"
            | Act a -> string a
            | Name s -> s
        let printGuard_ g _ =
            sprintf "%O %s %s" g tGUARD
        let rec printComp_ typ l = 
            let sep = 
                match typ with 
                | Seq -> sprintf  "%s " tSEQ
                | Choice -> sprintf " %s " tCHOICE
                | Par -> sprintf " %s " tPAR
            String.concat sep l
            |> if (Seq.length l) > 1 then (sprintf "(%s)") else id
        cata print_ printGuard_ printComp_ proc
    
    /// Simplifies the process by removing Comp
    /// elements with only one child.
    let simplify proc =
        let comp_ typ l =
            if List.length l = 1 then l.Head
            else Comp(typ, l)
        cata (BaseProcess) (fun g pos p -> Guard(g, p, pos)) comp_ proc
    
    let usedNames proc = 
        let used_ acc b = 
            match b.stmt with
            | Name n -> Set.add n acc
            | _ -> acc
        fold used_ (fun x _ -> x) (fun _ -> Seq.fold) Set.empty proc

    let recUsedNames (procs: Map<_, _>) name =
        let id2 x _ = x
        let rec used_ acc b = 
            match b.stmt with 
            | Name n when not <| Set.contains b acc -> 
                fold used_ id2 (fun _ -> Seq.fold)  (Set.add b acc) procs.[n]
            | _ -> acc
        fold used_ id2 (fun _ -> Seq.fold) Set.empty procs.[name]

    let private entryOrExit fn =
        let entrycomp_ = function
        | Seq -> fn
        | Choice | Par -> Set.unionMany
        cata Set.singleton (fun _ _ -> id) entrycomp_

    /// Returns the entry base processes of proc.
    let entry proc = entryOrExit List.head proc

    let exit proc = entryOrExit List.last proc

