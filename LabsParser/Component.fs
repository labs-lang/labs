module Components
open FParsec
open Types
open Processes
open Common

let plstigkeys = 
    choice [
        (betweenAng (pkeys L));
        (pinit L |>> List.singleton) >>= toMap
    ] |> sepbycommas |> ws

let pcomp = 
    pipe2
        ((ws (skipString "comp")) >>. ws IDENTIFIER)
        (betweenBraces (
            spaces
            >>. tuple4
                (opt (pstringEq "interface" (pkeys I)))
                (opt (pstringEq "stigmergy" plstigkeys))
                (pstringEq "behavior" (ws IDENTIFIER))
                processes))
        (fun n (i, l, p, procs) ->
            (n, {
                name = n
                iface= (i |> Option.defaultValue Map.empty)
                lstig= (l |> Option.defaultValue List.empty) 
                behavior = p
                processes = procs }))