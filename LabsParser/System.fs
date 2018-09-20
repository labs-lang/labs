module internal System
open FParsec
open Common
open Types
open Link
open Expressions
open Processes
open Components

let pspawn = 
    ws ((ws IDENTIFIER) .>>. (ws COLON >>. pint32)) |> sepbycommas >>= toMap

let plink =
    let linkref p =
        pipe3
            (ws KEYNAME)
            (opt (betweenBrackets p))
            (ws (skipString "of c") >>. 
                choice [charReturn '1' RefC1; charReturn '2' RefC2])
            (fun a b c -> {var=c(a); offset=b})
    let linkId = 
        (ws (skipString "id")) >>. 
        (ws (skipString "of c") >>. 
            choice [charReturn '1' Id1; charReturn '2' Id2])
    makeBExprParser (makeExprParser linkref linkId)

let plstig =

    let plstigkeys name = 
        let loc = L(name)
        choice [
            (betweenAng (pkeys loc));
            (pinit loc |>> List.singleton) >>= toMap
        ] |> sepbycommas |> ws

    (ws (skipString "stigmergy" |> ws) >>. (ws IDENTIFIER))
    >>= fun n ->
        preturn n
        .>>. ws (betweenBraces 
            ((pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS"))) <!> "LSTIG  "
    |>> fun (n, (l, v)) -> n, {name=n; link=l; vars=v}
    
let pextern = ws ((sepbycommas (skipChar '_' >>. KEYNAME))) |>> Set.ofList

let psys = 
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
            (opt (pstringEq "extern" (ws (skipRestOfLine true))))
            (opt (pstringEq "environment" (pkeys E)))
            (pstringEq "spawn" pspawn)
            (processes)
            (fun _ env spawn procs -> {
                processes = procs;
                environment = (env |> Option.defaultValue Map.empty);
                spawn = (makeRanges spawn);
                components = Map.empty;
                properties = Map.empty;
                stigmergies = Map.empty;
            })
        |> betweenBraces)
        |> ws
