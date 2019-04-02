module internal System
open FParsec

open LabsCore
open Tokens
open Init
open Types
open Processes
open Expressions

let pspawn =
    let pspawnexpr:Parser<Expr<unit,unit>> = 
        makeExprParser 
            (fun _ -> fail "unexpected variable in constant expression") 
            (skipString tID >>. notInIdentifier >>. fail "unexpected id in constant expression")
    
    (tuple3
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER .>> (ws (skipChar ':')))
        (ws pspawnexpr))
    |> sepbycommas

let pextern = (sepbycommas ((skipChar '_' >>. KEYNAME) |> ws) |> ws)

let psys =
    // TODO move away from here
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
            (opt (pstringEq "extern" pextern))
            (opt (pstringEq "environment" (pkeys Location.E)))
            (pstringEq "spawn" pspawn <!> "SPAWN")
            processes
            (fun ext env spawn procs -> {
                processes = procs
                externals = Option.defaultValue [] ext
                environment = Option.defaultValue [] env
                spawn = spawn
            })
        |> betweenBraces)
        |> ws
