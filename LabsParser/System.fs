module internal System
open FParsec

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
    
    (pipe3
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER .>> (ws (skipChar ':')))
        (ws pspawnexpr)
        (fun pos name expr -> {pos=pos; name=name; def=expr})
    )
    |> sepbycommas

let pextern = (sepbycommas ((skipChar '_' >>. KEYNAME) |> ws) |> ws)

let psys =
    (followedBy (skipString "system"))
    >>. getPosition
    .>> ws (skipString "system")
    .>>. 
        (pipe4
            (opt (pstringEq "extern" pextern) <!> "EXTERN")
            (opt (pstringEq "environment" (pkeys Location.E)) <!> "ENV")
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
    |>> (fun (pos, sys) -> {name="system"; pos=pos; def=sys})
