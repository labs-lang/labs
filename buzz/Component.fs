namespace Buzz
open System
open Buzz.LStig
    
    module Component = 
        [<StructuredFormatDisplay("{AsString}")>]
        type Comp = 
            public {
                L: LStig;
                I : Interface;
                P : Process; 
                _Id : Guid;
                _Stack : Key list
            }

            /// Returns a new component.
            static member Create() =
                {
                    Comp.L = LStig.Empty; 
                    P = Nil;
                    I = Map.empty<string, Val>;
                    _Id = Guid.NewGuid();
                    _Stack = []
                }

            member this.AsString =
                sprintf "{L=%A\nI=%A\nP=%A}\n" 
                    this.L this.I this.P
        
            member this.IsIdle() = this.P = Nil

            /// Implement semantics of components
            member this.Transitions() =

                // Some helper functions

                let EpsTr next =
                    (this, Eps, next)
                let WriteTr pair next =
                    (this, Write(this.I.["loc"], pair), next)
                let ReadTr pair next =
                    (this, Read(this.I.["loc"], pair), next)
                let WriteOrEps next stack pair =
                    if this.L.Accepts pair 
                    then [WriteTr pair {next with _Stack=stack; L=this.L + pair}]
                    else [EpsTr {next with _Stack=stack}]
                
                // Semantics of expressions
                let rec Eval e :(Val option * Set<Key>)=
                    match e with
                    | Const(c) -> Some(c), Set.empty<Key>
                    | L(k) -> 
                        if this.L.[k].IsSome 
                        then (Some(fst this.L.[k].Value) , Set.singleton k)
                        else failwith << sprintf "%s not tound" <| k.ToString()
                    | I(k) -> (Some this.I.[k], Set.empty<Key>)
                    | Sum(e1, e2) -> 
                        let (v1, s1) = Eval e1
                        let (v2, s2) = Eval e2
                        match (v1, v2) with
                        | (Some(x1), Some(x2)) -> 
                            match (x1 + x2) with
                            | Some(s) -> (Some(s), Set.union s1 s2)
                            | None -> (None, Set.empty)
                        | _ -> (None, Set.empty)                
                match this._Stack with
                | hd::tl -> 
                    let pair = (hd, this.L.[hd].Value)
                    [ReadTr pair {this with _Stack = tl}]
                | [] -> 
                    match this.P.Transition() with
                    | None -> []
                    | Some(action, next) ->
                        let nextThis = {this with P=next}
                        match action with
                        | Attr(a, e) -> 
                            let (v, keys) = Eval e
                            v
                            |> Option.bind (fun(x) -> [EpsTr {nextThis with I=this.I.Add(a, x); _Stack = List.ofSeq keys}] |> Some)
                            |> Option.defaultValue [EpsTr {this with P=Nil}]
                        | Put(pair) -> 
                            WriteOrEps nextThis [] pair
                        | Send(pair) ->
                            [(this, Write(this.I.["loc"], pair), nextThis)]
                        | LazyPut(k, e) ->
                            let (v, keys) = Eval e
                            let kList = List.ofSeq keys
                            v
                            |> Option.bind (fun(x) -> (k, (x, DateTime.Now)) |> WriteOrEps nextThis kList |> Some)
                            |> Option.defaultValue [EpsTr {this with P=Nil}]
                        | Await(k,v) ->
                            if this.Check(k,v) then
                                {this with P = next}.Transitions()
                                |> List.map (fun (c, l, nc) -> (this, l, nc))
                             else []

            member this.Check(k: Key, v: Val) =
                this.L.[k]
                |> Option.exists (fun p -> v.Equals(fst p))

        and CompTransition = Comp * Label * Comp
