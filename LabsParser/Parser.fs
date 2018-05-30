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
    //manyComments 
    spaces
    >>. tuple4 (processes) (ws ((pcomp |> ws |> many)>>= toMap)) psys pproperties
    |>> (fun (procs, comps, (env, spawn, link), props) -> 
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
    let pplaceholder = (skipChar '_') >>. KEYNAME
    manyTill
        (skipMany1Till skipAnyChar (eof <|> followedBy pplaceholder) >>. 
        ((followedBy eof >>. preturn "") <|> pplaceholder))
        (followedBy eof)