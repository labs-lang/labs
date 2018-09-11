module Parser

open FParsec
open Common
open Components
open System
open Properties

let makeRanges (mp: Map<'a, int>) =
    mp 
    |> Map.fold 
        (fun (c, m) name num -> 
            let newC = c + num
            (newC, (Map.add name (c, newC) m) )) (0, Map.empty) 
    |> snd
    
let parse = 
    spaces >>. pipe3
        psys 
        (ws ((pcomp |> ws |> many) >>= toMap))
        pproperties
        (fun (_, env, spawn, link, procs) comps props -> 
            {
            processes = procs;
            components = comps;
            environment = (env |> Option.defaultValue Map.empty);
            spawn = (makeRanges spawn);
            properties = props;
            link = link;
            })

let stripComments = 
    stringsSepBy (manySatisfy ((<>) '#')) (lineComment >>. preturn "")

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