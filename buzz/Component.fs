namespace Buzz
open System
open Buzz.LStig
open Buzz.Functions

    module Component = 
        [<StructuredFormatDisplay("{AsString}")>]
        type Comp = 
            public {
                K: LStig;
                I : Interface;
                P : Process; 
                _Id : Guid;
                _Stack : Key list
            }

            /// Returns a new component.
            static member Create() =
                {
                    Comp.K = LStig.Empty; 
                    P = Nil;
                    I = Map.empty<string, Val>;
                    _Id = Guid.NewGuid();
                    _Stack = []
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

                // Semantics of expressions
                let Eval e :(Val * Set<Key>)=
                    match e with
                    | Const(c) -> c, Set.empty
                    | K(k) -> 
                        if this.K.[k].IsSome 
                        then (fst this.K.[k].Value , Set.singleton k)
                        else failwith "%s not tound"
                    | I(k) -> (this.I.[k], Set.empty)
                
                match this._Stack with
                | hd::tl -> 
                    let pair = (hd, this.K.[hd].Value)
                    [(this, Read(this.I.["loc"], pair), {this with _Stack = tl})]
                | [] -> 
                    match this.P.Transition() with
                    | None -> []
                    | Some(action, next) ->
                        match action with
                        | Attr(a, e) ->
                            let (v, keys) = Eval e
                            [(this, Eps, {this with I=this.I.Add(a, v); P=next; _Stack = List.ofSeq keys})]
                        | Put(pair) -> 
                            PutTransition pair next
                        | Send(pair) ->
                            [(this, Write(this.I.["loc"], pair), {this with P=next})]
                        | LazyPut(k, e) ->
                            let (v, keys) = Eval e
                            let pair = (k, (v, DateTime.Now))
                            PutTransition pair next
                            |> List.map (fun (c,l,n) -> (c,l, {n with _Stack = List.ofSeq keys}))
                        | Await(k,v) ->
                            if this.Check(k,v) then
                                {this with P = next}.Transitions()
                                |> List.map (fun (c, l, nc) -> (this, l, nc))
                             else []

            member this.Check(k: Key, v: Val) =
                this.K.[k]
                |> Option.exists (fun p -> v.Equals(fst p))

        and CompTransition = Comp * Label * Comp
