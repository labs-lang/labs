module internal System
open FParsec
open Common

let pspawn = 
    let pnumber = 
        choice [
            pplaceholder |>> (resolvePlaceholder pint32); 
            pint32
        ]
    ws ((ws IDENTIFIER) .>>. pnumber) |> sepbycommas |> betweenBrackets

let psys = 
    ws (skipString "system")
    >>. ws (
        betweenBraces (
            spaces >>. manyComments
            >>. tuple2
                (ws (setDef "environment" KEYNAME id) .>> manyComments)
                (ws (skipString "spawn") >>. (ws EQ) >>. ws (pMap pspawn) .>> manyComments)))
