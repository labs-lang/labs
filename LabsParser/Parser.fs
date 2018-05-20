module Parser

open FParsec
open Common
open Processes
open Components
open System
open Properties

let makeRanges (mp: Map<'a, int>) =
    mp 
    |> Map.fold 
        (fun (c, m) name num -> 
            let newC = c+num
            (newC, (Map.add name (c, newC) m) )) (0, Map.empty) 
    |> snd

let parse = 
    manyComments 
    >>. tuple4 (processes) (ws (pMap (many (ws pcomp)))) psys pproperties
    .>> manyComments
    |>> (fun (procs, comps, (env, spawn, link), props) -> 
        {
        processes = procs;
        components = comps;
        environment = env;
        spawn = (makeRanges spawn);
        properties = props;
        link = link;
        })


let pre =
    let pplaceholder = (skipChar '&') >>. KEYNAME
    manyTill
        (skipMany1Till skipAnyChar (eof <|> followedBy pplaceholder) >>. 
        ((followedBy eof >>. preturn "") <|> pplaceholder))
        (followedBy eof)