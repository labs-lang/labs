module internal Stigmergies

open FParsec
open Types
open Link
open Common
open Expressions

let plink =
    let pc1orc2 = 
        (ws (ws (skipString "of")) >>. skipChar 'c' >>. 
            choice [charReturn '1' C1; charReturn '2' C2])
    let linkref p =
        pipe3
            (ws KEYNAME) (opt (betweenBrackets p)) pc1orc2
            (fun a b c -> {var=a,c; offset=b})
    let linkId = 
        (ws (skipString "id")) >>. pc1orc2
    makeBExprParser (makeExprParser linkref linkId)

let plstig =
    let ptuple loc =
        try
            pipe2
                (sepbycommas (pvar loc))
                ((ws COLON) >>. sepbycommas pinit)
                List.zip
                |>> List.map (fun (v, i) -> {v with init=i})
            >>= toSet byName byName
        with | :? System.ArgumentException ->
            fail "Tuples must contain the same numbers of variables and initializers."
    let plstigkeys name = 
        let loc = L name
        choice [
            followedBy ((pvar loc) >>. COMMA) >>. ptuple loc
            pinitdef loc |>> List.singleton >>= toSet byName byName
        ] |> sepbysemis |> ws

    (ws (skipString "stigmergy" |> ws) >>. (ws IDENTIFIER))
    >>= fun n ->
        preturn n
        .>>. ws (betweenBraces 
            ((pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS"))) <!> "LSTIG"
    |>> fun (n, (l, v)) -> n, {name=n; link=l; vars=v}
    