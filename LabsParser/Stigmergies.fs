module internal Stigmergies

open FParsec
open Types
open Link
open Common
open Expressions

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
    let ptuple loc =
        tuple2
            (sepbycommas (pvar loc))
            ((ws COLON) >>. sepbycommas pinit)
        >>= (fun (vars, inits) -> 
                List.zip vars inits |> Map.ofList |> preturn)

    let plstigkeys name = 
        let loc = L name
        choice [
            followedBy ((ws KEYNAME) >>. skipChar ',') >>. ptuple loc
            (pinitdef loc |>> List.singleton) >>= toMap
        ] |> sepbysemis |> ws

    (ws (skipString "stigmergy" |> ws) >>. (ws IDENTIFIER))
    >>= fun n ->
        preturn n
        .>>. ws (betweenBraces 
            ((pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS"))) <!> "LSTIG"
    |>> fun (n, (l, v)) -> n, {name=n; link=l; vars=v}
    