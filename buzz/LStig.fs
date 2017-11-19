namespace Buzz
open Buzz.Functions
    module LStig = 
        /// A map where each key maps to a pair (value, timestamp)
        [<StructuredFormatDisplay("{AsString}")>]
        type LStig = 
            private { d : Map<Key, Tval> }
           
            member this.AsString =
                this.d
                |> Map.toSeq
                |> Seq.map (fun (k, (v,_)) -> sprintf "%s=%A" k v)
                |> String.concat ","
                |> sprintf "{%s}"


            static member Empty = 
                {LStig.d = Map.empty<Key, Tval>}
            
            static member (!=) (left: LStig, right: LStig) = 
                not (left.d = right.d)

            static member (+) (left: LStig, right: Tpair) = 
                if left.Accepts right then {left with d = left.d.Add right } else left

            member this.Item
                with get(k) = this.d.TryFind(k)
            
            member this.TpairOf k =
                this.d.TryFind(k)
                |> Option.bind (fun p -> Some(k, p))

            member this.Accepts (p : Tpair) = 
                fst p
                |> this.d.TryFind
                |> Option.forall (fun v -> timeof (snd p) > timeof v)