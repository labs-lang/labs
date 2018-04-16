namespace Buzz
open System
    [<AutoOpen>]    
    module Types = 
        let rng = Random()

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

        type Tval = Val * int

        type Key = string
        type Tpair = Key * Tval

        type Interface = Map<Key, Val>

        type Label =
            | Eps
            | Write of Val * Tpair
            | Read of Val * Tpair
            | Qry of Interface * Tpair
            | Put of Interface * Tpair

        type Expr =
            | RandomPoint of xMin:int * yMin:int * xMax:int * yMax:int
            | Const of Val
            | L of Key
            | I of string
            | Sum of Expr * Expr

        ///<summmary>Boolean expression</summary>
        type Op = 
            | Equal
            | Less
            | Greater
        type BExpr =
            | True
            | False
            | Compare of Expr * Op * Expr
            | Neg of BExpr
            | Conj of BExpr * BExpr
            | NilCheck of Expr 

        type Action =
        | Attr of string * Expr
        //| Put of Tpair
        //| Send of Tpair
        | LazyPut of Key * Expr
        // TODO: it might be better to create a generic Await with a dedicated
        // expression type. Something like
        // <x OP>
        // OP ::= NIL | = v | !OP | OP /\ OP
        | Await of BExpr
        //| AwaitNot of Key * Val
        with
            override this.ToString() = 
                match this with
                | Attr(a, e) -> sprintf "(%s := %s)" a (e.ToString())
                //| Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
                //| Send(p) -> sprintf "!(%s=%A)" (fst p) (fst (snd p))
                | LazyPut(k, v) -> sprintf "{%s <- %A}" k v
                | Await(b) -> sprintf "<%A>" b
                //| AwaitNot(k, v) -> sprintf "<%A != %A>" k v

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

                /// Returns the process r where all occurrences of X are
                /// replaced by x
                let rec unwind (x:recProcess) r = 
                    match r with
                    | RNil -> Nil
                    | X -> RecX(x)
                    | RSeq(a, p) -> Seq(a, unwind x p)
                    | RChoice(p1, p2) -> Choice(unwind x p1, unwind x p2)
                    // rec x is idempotent: rec x. (rec x. P) = rec x. P
                    | RRec(p) -> unwind p p

                match this with
                | Nil -> Seq.empty
                | Seq(a, p) -> Seq.singleton (a, p)
                | Choice(p, q) -> Seq.append p.Commitments q.Commitments
                | RecX(r) -> (unwind r r).Commitments
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
        | RRec of recProcess
        | X
        with
            static member ( + )(left: recProcess, right: recProcess) =
                RChoice(left, right)
            static member ( ^. ) (left: Action, right: recProcess) =
                RSeq(left, right)
            member this.AsString = this.ToString()
            override this.ToString() =
                match this with
                | RNil -> "0"
                | X -> "X"
                | RSeq(a, p) -> sprintf "%s.%s" (a.ToString()) p.AsString
                | RChoice(p, q) -> sprintf "%s + %s" p.AsString q.AsString
                | RRec(p) -> sprintf "recX.%s" p.AsString

        let makeClock() =
            let x = ref 0
            let tick() =
                x := !x + 1
                !x
            tick
        
        let globalClock = makeClock()