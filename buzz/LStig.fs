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
                |> Seq.map (fun (k, (v,_)) -> sprintf "%A=%A)" k v)
                |> String.concat ","
                |> sprintf "{%s}"


            static member Empty = 
                {LStig.d = Map.empty<Key, Tval>}
            
            static member (!=) (left: LStig, right: LStig) = 
                not (left.d = right.d)

            static member (+) (left: LStig, right: Tpair) = 
                let isNewer = 
                    right
                    |> fst
                    |> left.d.TryFind 
                    |> Option.forall (fun v -> timeof (snd right) > timeof v)
                if isNewer then {left with d = left.d.Add right } else left

            member this.Item
                with get(k) = this.d.TryFind(k)
