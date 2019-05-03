namespace LabsCore
open Tokens
open Types
open FSharpPlus.Lens
open FParsec

type Stmt<'a> = 
    | Nil 
    | Skip
    | Act of 'a Action
    | Name of string
with
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Skip -> "√"
        | Act a -> string a
        | Name s -> s

//type Base<'a, 'b> =
//    { stmt: Stmt<'a>; pos: 'b }

type Composition =
        | Seq
        | Choice
        | Par

type Process<'a> =
    | BaseProcess of Node<Stmt<'a>> //Base<'a, 'b>
    | Guard of Node<BExpr<'a, unit> * Process<'a>>
    | Comp of Composition * Process<'a> list

module Process =
    let rec fold fbase fguard fcomp acc proc =
        let recurse = fold fbase fguard fcomp
        match proc with
        | BaseProcess b ->
            fbase acc b
        | Guard n  ->
            recurse (fguard acc (fst n.def)) (snd n.def)
        | Comp(typ, l) -> 
            fcomp typ recurse acc l
    
    let rec cata fbase fguard fcomp proc = 
        let recurse = cata fbase fguard fcomp
        match proc with
        | BaseProcess b -> fbase b
        | Guard(n) ->
            fguard n (recurse (snd n.def))
        | Comp(typ, l) -> fcomp typ (l |> List.map recurse)

    let rec map fbase fguard proc =
        let recurse = map fbase fguard
        match proc with
        | BaseProcess b -> fbase b
        | Guard(n) ->
            let g, p = n.def
            let p' = recurse p
            Guard(setl _def (fguard g, p') n)
        | Comp(typ, l) -> Comp(typ, List.map recurse l)

    let rec print proc =
        let print_ b = string b.def
//            match b.def with
//            | Nil -> "0"
//            | Skip -> "√"
//            | Act a -> string a
//            | Name s -> s
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
    
    /// Simplifies the process by removing Comp
    /// elements with only one child.
    let simplify proc =
        let comp_ typ l =
            if List.length l = 1 then l.Head
            else Comp(typ, l)
        let guard_ n p =
            Guard(setl (_def << _2) p n)
        cata (BaseProcess) guard_ comp_ proc
    
    let usedNames proc = 
        let used_ acc b = 
            match b.def with
            | Name n -> Set.add (n, b.pos) acc
            | _ -> acc
        fold used_ (fun x _ -> x) (fun _ -> Seq.fold) Set.empty proc

    let recUsedNames (procs: Map<_, _>) name =
        let id2 x _ = x
        let rec base_ acc b = 
            match b.def with 
            | Name n when not <| Set.contains b acc -> 
                fold base_ id2 (fun _ -> Seq.fold) (Set.add b acc) procs.[n]
            | _ -> acc
        fold base_ id2 (fun _ -> Seq.fold) Set.empty procs.[name]

    let private entryOrExit fn =
        let entrycomp_ = function
        | Seq -> fn
        | Choice | Par -> Set.unionMany
        cata Set.singleton (fun _ -> id) entrycomp_

    /// Returns the entry base processes of proc.
    let entry proc = entryOrExit List.head proc

    /// Returns the entry base processes of proc.
    let exit proc = entryOrExit List.last proc

    let tag name pos def =
        let p=def.pos in let p' = Position((sprintf "%s@%O" name pos), p.Index, p.Line, p.Column)
        setl _pos p' def
    
    /// Replace (non-recursive) Name processes with their definitions.
    // Inserts information about the Name process into its expansion's elements.
    let expand (procs: Map<_, _>) name =
        let rec expand_ visited name = 
            let base_ b = 
                match b.def with
                | Name n when n=name || n="Behavior" -> BaseProcess b
                | Name n when (not (Set.contains b visited)) ->
                    match procs.TryFind n with
                    | Some _ -> 
                        expand_ (visited.Add b) n 
                        |> map ((tag n b.pos) >> BaseProcess) id
                    | None -> failwith n
                | _ -> BaseProcess b
            map base_ id procs.[name]
        expand_ Set.empty name