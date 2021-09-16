module Components
open FParsec
open LabsCore.Grammar
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
        (fun pos n (i, l, procs) -> {
            Pos=pos;
            Name=n
            Source="";
            Def = {
                Name = n
                Iface = Option.defaultValue [] i
                Lstig = Option.defaultValue [] l 
                Processes = procs
        }})