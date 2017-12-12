namespace Buzz
open System
open Buzz.LStig
open Buzz.Expressions
    
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
                    then WriteTr pair {next with _Stack=stack; L=this.L + pair}
                    else EpsTr {next with _Stack=stack}
                
                let MatchCommitment (action, next) : (Comp * Label * Comp) list=
                    let nextThis = {this with P=next}
                    match action with
                    | Attr(a, e) -> 
                        let ks = List.ofSeq <| keys e
                        eval e this.I this.L
                        |> Option.bind  (fun v ->
                            Some <| [EpsTr {nextThis with I=this.I.Add(a, v); _Stack = ks}])
                        |> Option.defaultValue [EpsTr {this with P=Nil}]
                    | Send(pair) ->
                        [(this, Write(this.I.["loc"], pair), nextThis)]
                    | Put(pair) -> 
                        [WriteOrEps nextThis [] pair]
                    | LazyPut(k, e) ->
                        let ks = List.ofSeq <| keys e
                        eval e this.I this.L
                        |> Option.bind (fun(x) -> 
                            (k, (x, DateTime.Now)) 
                            |> WriteOrEps nextThis ks
                            |> List.singleton
                            |> Some)
                        |> Option.defaultValue [EpsTr {this with P=Nil}]
                    | Await(k,v) ->
                        match this.L.[k] with
                        | Some(w, _) when v = w ->
                            {this with P = next}.Transitions()
                            |> List.map (fun (c, l, nc) -> (this, l, nc))
                        | _ -> []
                    | AwaitNot(k,v) -> 
                        match this.L.[k] with
                        | Some(w, _) when v = w -> []
                        | _ -> 
                            {this with P = next}.Transitions()
                            |> List.map (fun (c, l, nc) -> (this, l, nc))

                match this._Stack with
                | hd::tl -> 
                    let pair = (hd, this.L.[hd].Value)
                    [ReadTr pair {this with _Stack = tl}]
                | [] -> 
                    this.P.Commitments
                    |> Seq.map MatchCommitment
                    |> List.concat

        and CompTransition = Comp * Label * Comp
