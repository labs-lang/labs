namespace Buzz
open System
    [<AutoOpen>]    
    module Types = 
        type Point = int * int

        type Val =
            | Int of int
            | String of string
            | P of Point
            static member (+) (left: Val, right: Val) =
                match (left, right) with
                | (Int(a), Int(b)) -> Some(Int(a+b)) // Sum
                | (P(p1), P(p2)) -> Some(P(fst p1 + fst p2, snd p1 + snd p2))
                | (String(a), String(b)) -> Some(String(a + b)) // Concatenation
                | _ -> None

            static member (-) (left: Val, right: Val) =
                match (left, right) with
                | (Int(a), Int(b)) -> Some(Int(a-b))
                | (P(p1), P(p2)) -> Some(P(fst p1 - fst p2, snd p1 - snd p2))
                | _ -> None

        type Tval = Val * DateTime

        type Key = string
        type Tpair = Key * Tval

        type Label =
            | Eps
            | Write of Val * Tpair
            | Read of Val * Tpair

        type Interface = Map<Key, Val>

        type Expr =
            | Const of Val
            | L of Key
            | I of string
            | Sum of Expr * Expr

        type Action =
        | Attr of string * Expr
        | Put of Tpair
        | Send of Tpair
        | LazyPut of Key * Expr
        | Await of Key * Val
        with
            override this.ToString() = 
                match this with
                | Attr(a, e) -> sprintf "(%s := %s)" a (e.ToString())
                | Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
                | Send(p) -> sprintf "!(%s=%A)" (fst p) (fst (snd p))
                | LazyPut(k, v) -> sprintf "{%s <- %A}" k v
                | Await(k, v) -> sprintf "<%A = %A>" k v

        [<StructuredFormatDisplay("{AsString}")>]
        type Process = 
        | Nil
        | Seq of Action * Process
        | Choice of Process * Process
        | RecX of recProcess
        with
            static member ( ^. )(left: Action, right: Process) =
                Seq(left, right)
            static member ( + )(left: Process, right: Process) =
                Choice(left, right)

            member this.Commitments = 
                /// Returns a recProcess where all occurrences of X are 
                /// replaced by r itself.

                match this with
                | Nil -> []
                | Seq(a, p) -> [(a, p)]
                | Choice(p, q) -> List.append p.Commitments q.Commitments
                | RecX(r) -> r.Commitments
                member this.AsString = this.ToString()        
                override this.ToString() =
                    match this with
                    | Nil -> "0"
                    | Seq(a, p) -> sprintf "%s.%s" (a.ToString()) p.AsString
                    | Choice(p, q) -> sprintf "%s + %s" p.AsString q.AsString
                    | RecX(r) -> sprintf "recX.(%s)" r.AsString

        and recProcess =
        | RNil
        | RSeq of Action * recProcess
        | RChoice of recProcess * recProcess
        | X
        with
            static member ( + )(left: recProcess, right: recProcess) =
                RChoice(left, right)
            static member ( ^. ) (left: Action, right: recProcess) =
                RSeq(left, right)
            member this.Commitments =
                let rec replace x r = 
                    match r with
                    | RNil -> RNil
                    | X -> x
                    | RSeq(a, p) -> RSeq(a, replace x p)
                    | RChoice(p1, p2) -> RChoice(replace x p1, replace x p2)

                match this with
                | RNil
                | X -> []
                | RSeq(a, p) -> [(a, RecX(replace this p))]
                | RChoice(p, q) -> List.append p.Commitments q.Commitments

            member this.AsString = this.ToString()
            override this.ToString() =
                match this with
                | RNil -> "0"
                | X -> "X"
                | RSeq(a, p) -> sprintf "%s.%s" (a.ToString()) p.AsString
                | RChoice(p, q) -> sprintf "%s + %s" p.AsString q.AsString