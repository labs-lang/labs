module internal System
open FParsec
open Common
open Types
open Link
open Expressions
open Processes

let pspawn = 
    ws ((ws IDENTIFIER) .>>. (ws COLON >>. pint32)) |> sepbycommas >>= toMap


let linkref p =
    pipe3
        (ws KEYNAME)
        (opt (betweenBrackets p))
        (ws (skipString "of c") >>. 
            choice [charReturn '1' RefC1; charReturn '2' RefC2])
        (fun a b c -> {var=c(a); offset=b})

let plink = makeBExprParser (makeExprParser linkref)

let pextern = ws ((sepbycommas (skipChar '_' >>. KEYNAME))) |>> Set.ofList

let psys = 
    ws (skipString "system")
    >>. 
        (tuple5
            (opt (pstringEq "extern" (ws (skipRestOfLine true))))
            (opt (pstringEq "environment" (pkeys E)))
            (pstringEq "spawn" pspawn)
            (pstringEq "link" plink)
            (processes)
        |> betweenBraces)
        |> ws
