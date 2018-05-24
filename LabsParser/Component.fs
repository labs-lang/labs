module Components
open FParsec
open Processes
open Common

let plstigkeys = 
    choice [
        (betweenAng pkeys);
        (pinit |>> List.singleton) >>= toMap
    ] |> sepbycommas |> ws

let pcomp = 
    (ws (skipString "comp"))
    >>. ws IDENTIFIER
    .>>. betweenBraces (
        spaces
        >>. tuple4
            (opt (pstringEq "interface" pkeys))
            (opt (pstringEq "stigmergy" plstigkeys))
            (pstringEq "behavior" (ws IDENTIFIER))
            processes)
    |>> (fun (n, (i,l,p,procs)) ->
        (n, {
            name = n; 
            iface= (i |> Option.defaultValue Map.empty);
            lstig= (l |> Option.defaultValue List.empty); 
            behavior = p; 
            processes = procs}))