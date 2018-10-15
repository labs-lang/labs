module internal System
open FParsec
open Common
open Types
open Processes

let pspawn = 
    ws ((ws IDENTIFIER) .>>. (ws COLON >>. pint32)) |> sepbycommas >>= toMap

let pextern = ws ((sepbycommas (skipChar '_' >>. KEYNAME))) |>> Set.ofList

let psys = 
    let makeRanges (mp: Map<'a, int>) =
        mp 
        |> Map.fold 
            (fun (c, m) name num -> 
                let newC = c + num
                (newC, (Map.add name (c, newC) m) )) (0, Map.empty) 
        |> snd

    ws (skipString "system")
    >>. 
        (pipe4
            (opt (pstringEq "extern" (ws (skipRestOfLine true))))
            (opt (pstringEq "environment" (pkeys E)))
            (pstringEq "spawn" pspawn)
            (processes)
            (fun _ env spawn procs -> {
                processes = procs;
                environment = (env |> Option.defaultValue Map.empty);
                spawn = (makeRanges spawn);
                components = Map.empty;
                properties = Map.empty;
                stigmergies = Map.empty;
            })
        |> betweenBraces)
        |> ws
