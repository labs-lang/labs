namespace Buzz
open Buzz.Component
open Chiron

    module Json = 
        let ValToJson = function
        | Int(i) -> (decimal >> Number) i
        | Val.String(s) -> Chiron.String s
        | P(x,y) -> [x;y] |> List.map (decimal >> Number) |> Array

        let compToJson (c:Comp) =
            let i = 
                c.I 
                |> Map.map (fun k -> ValToJson)
                |> Object
            let l =
                c.L.getMap
                |> Map.map (fun k (v, _) -> ValToJson v)
                |> Object
            Map.ofSeq ["L", l; "I", i; "id", c._Id.ToString() |> String] |> Object