namespace Buzz
open Buzz.Component
open Buzz.System
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
                |> Map.map (fun k -> ValToJson << fst)
                |> Object
            Map.ofSeq ["L", l; "I", i; "id", c._Id.ToString() |> String] |> Object


        let sysToJson (s:Sys) =
            s 
            |> Set.map compToJson
            |> Set.toList
            |> Array

        let traceToJson (t: TraceStep list) = 
            t
            |> List.map (sysToJson << fun (_,s,_) -> s)
            |> Array
