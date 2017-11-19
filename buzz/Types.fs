namespace Buzz
open System
    [<AutoOpen>]    
    module Types = 
        type Point = int * int

        type Val =
            | Int of int
            | String of string
            | P of Point

        type Tval = Val * DateTime

        type Key = string
        type Tpair = Key * Tval

        type Label =
            | Eps
            | Write of Val * Tpair
            | Read of Val * Tpair

        type Interface = Map<string, Val>

        type Expr =
            | Const of Val
            | K of Key
            | I of string

        [<StructuredFormatDisplay("{AsString}")>]
        type Process = 
            | Nil
            | Term
            | Act of Action
            | Seq of Process * Process
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
                | Act(a) -> a.ToString()
                | Star(proc) -> sprintf "(%s)*" proc.AsString

            // Structural semantics of processes
            member this.Transition() :(Action * Process) option =
                match this with
                | Nil
                | Term -> None
                | Seq(Nil, _) -> None
                | Star(Nil) -> None
                // act
                | Act(a) -> Some(a, Nil)
                // This pattern encodes both seq1 and seq2
                | Seq (Term, p) -> p.Transition()
                | Seq(p1, p2) ->
                    p1.Transition()
                    |> Option.bind (fun (l, next) -> Some(l, if next = Nil then p2 else next ^. p2) )
                // star1 and star2
                | Star(p) -> 
                    p.Transition()
                    |> Option.bind (fun (l, next) -> Some(l, if next = Nil then this else next ^. this))
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