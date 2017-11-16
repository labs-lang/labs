namespace Buzz
open System
    [<AutoOpen>]    
    module Types = 
        type Point = int * int
        type AttrVal =
            | Int of int
            | String of string
            | P of Point

        type Val = int
        type Tval = Val * DateTime

        type Key = string
        type Tpair = Key * Tval

        type Label =
            | Eps
            | Write of AttrVal * Tpair
            | Read of AttrVal * Tpair

        type Interface = Map<string, AttrVal>

       
        
        [<StructuredFormatDisplay("{AsString}")>]
        type Process = 
            | Nil
            | Act of Action
            //| Proxy of string * Lazy<Process>
            | Seq of Process * Process
            | Star of Process
            static member ( ^. )(left: Process, right: Process) =
                match left with
                | Nil -> left       // 0.P == 0
                | Star(_) -> left   // P*.Q == P*
                | _ -> Seq(left, right)
            static member ( ^. )(left: Action, right: Process) =
                Seq(Act(left), right)

            member this.AsString = this.ToString()        
            override this.ToString() =
                match this with
                | Nil -> "0"
                //| Proxy(name, p) -> name 
                | Seq(action, proc) -> sprintf "%s.%s" (action.ToString()) proc.AsString
                | Act(a) -> a.ToString()
                | Star(proc) -> sprintf "(%s)*" proc.AsString

            // Structural semantics of processes
            member this.Transition() :(Action * Process) option =
                match this with
                | Nil -> None
                | Seq(Nil, p) -> None
                | Star(Nil) -> None
                // act
                | Act(a) -> Some(a, Nil)
                // This pattern encodes both seq1 and seq2
                | Seq(p1, p2) ->
                    p1.Transition()
                    |> Option.bind (fun (l, next) -> Some(l, if next = Nil then p2 else next ^. p2) )
                // star1 and star2
                | Star(p) -> 
                    p.Transition()
                    |> Option.bind (fun (l, next) -> Some(l, if next = Nil then this else next ^. this))
                //| Proxy(_,_) -> None
        and Action =
        | Attr of string * AttrVal
        | Put of Tpair
        | LazyPut of Key * Val
        | Await of Key * Val
        with
        static member ( ^. )(left: Action, right: Action) =
            Seq(Act(left), Act(right))
        override this.ToString() = 
            match this with
            | Attr(a, v) -> sprintf "[%s := %s]" a (v.ToString())
            | Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
            | LazyPut(k, v) -> sprintf "{%s <- %A}" k v
            | Await(k, v) -> sprintf "<%A = %A>" k v