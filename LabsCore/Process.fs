module Process
open Types
open Tokens

let rec fold fbase acc proc = 
    match proc with
    | BaseProcess b ->
        fbase acc b
    | Paren p
    | Guard(_, p, _) -> fold fbase acc p
    | Comp(_, l) -> 
        Seq.fold (fold fbase) acc l

let rec cata fbase fguard fcomp proc = 
    let recurse = cata fbase fguard fcomp
    match proc with
    | BaseProcess b -> fbase b
    | Guard(g, p, _) -> fguard g (recurse p)
    | Paren p -> recurse p
    | Comp(typ, l) -> fcomp typ (l |> List.map recurse)

let rec map fbase proc =
    match proc with
    | BaseProcess b -> fbase b
    | Guard(g, p, pos) -> Guard(g, (map fbase p), pos)
    | Paren p -> Paren (map fbase p)
    | Comp(typ, l) -> Comp(typ, List.map (map fbase) l)

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
        | Name n -> 
            Set.add n acc
        | _ -> acc
    fold used_ Set.empty proc

let recUsedNames (procs: Map<_, _>) name =
    let rec used_ acc b = 
        match b.stmt with 
        | Name n when not <| Set.contains b acc -> 
            fold used_ (Set.add b acc) procs.[n]
        | _ -> acc
    fold used_ Set.empty procs.[name]
    
let isRecursive procs name =
    recUsedNames procs name
    |> Set.map (fun x -> match x.stmt with Name n -> n | _ -> "")
    |> Set.contains name

let entry proc =
    let entrycomp_ = function
    | Seq -> List.head
    | Choice | Par -> Set.unionMany
    cata Set.singleton (fun _ -> id) entrycomp_ proc
    