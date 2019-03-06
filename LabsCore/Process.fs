﻿namespace LabsCore
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
    let rec fold fbase acc proc = 
        match proc with
        | BaseProcess b ->
            fbase acc b
        | Guard(_, p, _) -> fold fbase acc p
        | Comp(_, l) -> 
            Seq.fold (fold fbase) acc l

    let rec mfold fbase fguard fcomp acc proc =
        let recurse = mfold fbase fguard fcomp
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
        | Guard(g, p, _) -> fguard g (recurse p)
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
        let printGuard_ g =
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

    let usedNames proc = 
        let used_ acc b = 
            match b.stmt with
            | Name n -> Set.add n acc
            | _ -> acc
        fold used_ Set.empty proc

    let recUsedNames (procs: Map<_, _>) name =
        let rec used_ acc b = 
            match b.stmt with 
            | Name n when not <| Set.contains b acc -> 
                fold used_ (Set.add b acc) procs.[n]
            | _ -> acc
        fold used_ Set.empty procs.[name]

    let private entryOrExit fn =
        let entrycomp_ = function
        | Seq -> fn
        | Choice | Par -> Set.unionMany
        cata Set.singleton (fun _ -> id) entrycomp_

    /// Returns the entry base processes of proc.
    let entry proc = entryOrExit List.head proc

    let exit proc = entryOrExit List.last proc

    let name_ x = match x.stmt with Name n -> n | _ -> ""
    let isRecursive procs name =
        recUsedNames procs name
        |> Set.map name_
        |> Set.contains name