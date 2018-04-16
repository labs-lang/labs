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
                _StackP : Set<Key>;
                _StackQ : Set<Key>;
            }


            /// Returns a new component.
            static member Create() =
                {
                    Comp.L = LStig.Empty; 
                    P = Nil;
                    I = Map.empty<string, Val>;
                    _Id = Guid.NewGuid();
                    _StackP = Set.empty;
                    _StackQ = Set.empty
                }

            member this.AsString =
                sprintf "{L=%A\nI=%A\nP=%A}\n" 
                    this.L this.I this.P
        
            member this.IsIdle() = this.P = Nil

            /// Implement semantics of components
            member this.Transitions() =
                let EpsTr (next:Comp) =
                    (this, Eps, next)
                let TryEval expr comp : (Comp * Val) option =
                    eval expr comp.I comp.L
                    |> Option.bind (fun v -> 
                        Some({comp with _StackQ = Set.union this._StackQ (keys expr)}, v))

                let ProcessTransition (action, next) : (Comp * Label * Comp) list =
                    let nextThis = {this with P=next}
                    match action with
                    | Attr(k, e) -> 
                        let newComp = 
                            TryEval e this
                            |> Option.bind (fun (c, v) -> Some {c with P=next; I=this.I.Add(k, v)})
                            // TODO: If the eval fails, the component terminates
                            |> Option.defaultValue {this with P=Nil}
                        [EpsTr newComp]
                    | LazyPut(k, e) ->
                        let newComp = 
                            TryEval e this
                            |> Option.bind (fun (c, v) -> 
                                let t = globalClock()
                                Some {c with P=next; L=this.L + (k, (v, t))})
                            // TODO: If the eval fails, the component terminates
                            |> Option.defaultValue {this with P=Nil}
                        [EpsTr newComp]
                     | Await(b) ->
                         if beval b this.I this.L then
                             {this with P = next}.Transitions()
                             |> List.map (fun (c, l, nc) -> (this, l, nc))
                         else []

                this.P.Commitments
                |> Seq.map ProcessTransition
                |> List.concat
        and CompTransition = Comp * Label * Comp
