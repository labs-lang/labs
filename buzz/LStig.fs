namespace Buzz
open System
open Buzz.Functions
    module LStig = 
    /// A timestamped map. Each key has one pair (value, timestamp)
        type LStig = 
            private { d : Map<Key, Tval> }
           
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
