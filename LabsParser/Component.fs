module Components
open FParsec
open Types
open Processes
open Init

let pcomp =
    (ws (skipString "agent"))
    >>. pipe3
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER)
        ((tuple3
            (opt (pstringEq "interface" (pkeys I)))
            (opt (pstringEq "stigmergies" (ws IDENTIFIER |> sepbysemis)))
            processes <!> "PROCESSES") |> betweenBraces)
        (fun pos n (i, l, procs) ->
            {
                pos = pos
                name = n
                iface = Option.defaultValue [] i
                lstig = Option.defaultValue [] l 
                processes = procs })