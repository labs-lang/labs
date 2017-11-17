namespace Buzz
open System
open Buzz.LStig
open Buzz.Functions

    module Component = 
        [<StructuredFormatDisplay("{AsString}")>]
        type Comp = 
            public { K: LStig; I : Interface; P : Process; _Id : Guid }

            /// Returns a new component.
            static member Create() =
                {
                    Comp.K = LStig.Empty; 
                    P = Nil;
                    I = Map.empty<string, AttrVal>;
                    _Id = Guid.NewGuid()
                }

            member this.AsString =
                sprintf "{K=%A\nI=%A\nP=%A}\n" 
                    this.K this.I this.P
        
            member this.IsIdle() = this.P = Nil

            /// Implement semantics of components
            member this.Transitions() =

                let PutTransition pair nextP =
                    let next = {this with P=nextP}
                    if this.K=this.K + pair 
                    then [(this, Eps, next)]
                    else [(this, Write(this.I.["loc"], pair), {next with K=this.K + pair;})]

                let Eval e: AttrVal =
                    match e with
                    | Const(c) -> c
                    | K(k) -> 
                        if this.K.[k].IsSome 
                        then fst (this.K.[k].Value) 
                        else failwith "%s not tound"
                    | I(k) -> this.I.[k]


                match this.P.Transition() with
                | None -> []
                | Some(action, next) ->
                    match action with
                    | Attr(a, v) ->
                        [(this, Eps, {this with I=this.I.Add(a, Eval v); P=next})]
                    | Put(pair) -> 
                        PutTransition pair next
                    | LazyPut(k, e) ->
                        let pair = (k, (Eval e, DateTime.Now))
                        PutTransition pair next
                    | Await(k,v) ->
                        if this.Check(k,v) then
                            {this with P = next}.Transitions()
                            |> List.map (fun (c, l, nc) -> (this, l, nc))
                         else []

            member this.Check(k: Key, v: AttrVal) =
                this.K.[k]
                |> Option.exists (fun p -> v.Equals(fst p))

        and CompTransition = Comp * Label * Comp
