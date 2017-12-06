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

        [<StructuredFormatDisplay("{AsString}")>]
        type Process = 
            | Nil
            | Term
            | Act of Action
            | Seq of Process * Process
            | Choice of Process * Process
            | Star of Process
            static member ( ^. )(left: Process, right: Process) =
                match left with
                | Nil -> left       // 0.P == 0
                | Term -> right     // 1.P == P
                | Star(_) -> left   // P*.Q == P*
                | _ -> Seq(left, right)
            static member ( ^. )(left: Action, right: Process) =
                Seq(Act(left), right)

            member this.AsString = this.ToString()        
            override this.ToString() =
                match this with
                | Nil -> "0"
                | Term -> "1"
                | Seq(action, proc) -> sprintf "%s.%s" (action.ToString()) proc.AsString
                | Choice(p1,p2) -> sprintf "%s + %s" p1.AsString p2.AsString
                | Act(a) -> a.ToString()
                | Star(proc) -> sprintf "(%s)*" proc.AsString

            // Operational semantics of processes
            member this.Commitments =
                match this with
                | Nil
                | Term -> []
                | Seq(Nil, _) -> []
                | Star(Nil) -> []
                // act
                | Act(a) -> [(a, Term)]
                // This pattern encodes both seq1 and seq2
                | Seq (Term, p) -> p.Commitments
                | Choice(p1, p2) -> List.append p1.Commitments p2.Commitments
                | Seq(p1, p2) ->
                    p1.Commitments
                    |> List.map (fun (l, next) -> (l, if next = Term then p2 else next ^. p2) )
                // star1 and star2
                | Star(p) -> 
                    p.Commitments
                    |> List.map (fun (l, next) -> (l, if next = Term then this else next ^. this))
        and Action =
        | Attr of string * Expr
        | Put of Tpair
        | Send of Tpair
        | LazyPut of Key * Expr
        | Await of Key * Val
        with
        static member ( ^. )(left: Action, right: Action) =
            Seq(Act(left), Act(right))
        override this.ToString() = 
            match this with
            | Attr(a, e) -> sprintf "(%s := %s)" a (e.ToString())
            | Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
            | Send(p) -> sprintf "!(%s=%A)" (fst p) (fst (snd p))
            | LazyPut(k, v) -> sprintf "{%s <- %A}" k v
            | Await(k, v) -> sprintf "<%A = %A>" k v