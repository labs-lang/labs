module Parser

open FParsec
open Common
open Components
open System
open Stigmergies
open Properties
 
let parse = 
    spaces >>. pipe4
        psys 
        (ws ((plstig |> ws |> many) >>= toMap))
        (ws ((pcomp |> ws |> many) >>= toMap))
        pproperties
        (fun sys lstigs comps props -> 
            {sys with
                components = comps
                properties = props
                stigmergies = lstigs
            })

let stripComments = 
    stringsSepBy (manySatisfy ((<>) '#')) (lineComment >>% "")

let pre =
    attempt (skipMany1Till skipAnyChar (eof <|> followedBy (pstring "extern"))
    >>. (pstringEq "extern" (ws pextern))
    .>> skipMany1Till skipAnyChar (eof)) <|> preturn Set.empty

let allPlaceholders =
    let pplaceholder = (skipChar '_') >>. KEYNAME
    manyTill
        (skipMany1Till skipAnyChar (eof <|> followedBy pplaceholder) >>. 
        ((followedBy eof >>. preturn "") <|> pplaceholder))
        (followedBy eof)