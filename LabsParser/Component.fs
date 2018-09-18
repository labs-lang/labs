module Components
open FParsec
open Types
open Processes
open Common

let pcomp = 
    pipe2
        ((ws (skipString "agent")) >>. ws IDENTIFIER)
        (betweenBraces (
            spaces
            >>. tuple3
                (opt (pstringEq "interface" (pkeys I)))
                (opt (pstringEq "stigmergies" (ws IDENTIFIER |> sepbycommas)))
                processes <!> "PROCESSES"))
        (fun n (i, l, procs) ->
            (n, {
                name = n
                iface= (i |> Option.defaultValue Map.empty)
                lstig= (l |> Option.defaultValue List.empty) 
                processes = procs }))