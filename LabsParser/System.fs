module internal System
open FParsec

open LabsCore.Grammar
open LabsCore.Tokens
open Init
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
        (fun pos name expr -> {Pos=pos; Name=name; Def=expr; Source=""})
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
                Processes = procs
                Externals = Option.defaultValue [] ext
                Environment = Option.defaultValue [] env
                Spawn = spawn
            })
        |> betweenBraces)
        |> ws
    |>> (fun (pos, sys) -> {Name="system"; Pos=pos; Def=sys; Source=""})
