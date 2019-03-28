module Components
open FParsec
open Types
open Processes
open Init

let pcomp = 
    pipe2
        ((ws (skipString "agent")) >>. ws IDENTIFIER)
        (betweenBraces (
            tuple3
                (opt (pstringEq "interface" (pkeys I)))
                (opt (pstringEq "stigmergies" (ws IDENTIFIER |> sepbysemis)))
                processes <!> "PROCESSES"))
        (fun n (i, l, procs) ->
            (n, {
                name = n
                iface= (i |> Option.defaultValue Set.empty)
                lstig= (l |> Option.defaultValue List.empty) 
                processes = procs }))