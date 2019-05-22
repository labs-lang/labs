module internal Stigmergies

open FParsec
open Types
open Init
open Expressions
open Tokens

let plink =
    let pc1orc2 = 
        (ws (ws OF) >>. skipChar 'c' >>. choice [charReturn '1' C1; charReturn '2' C2])
    let linkref p =
        pipe3
            (ws KEYNAME) (opt (betweenBrackets p)) pc1orc2
            (fun a b c -> {var=a,c; offset=b})
    let linkId = (ws (skipString tID)) >>. pc1orc2
    getPosition .>>. makeBExprParser (makeExprParser linkref linkId)
    |>> (fun (pos, link) -> {name="link"; pos=pos; def=link})

let plstig : Parser<_> =
    let ptuple name =
        let loc = L(name, 0)
        try
            pipe2
                (sepbycommas (pvar loc))
                ((ws COLON) >>. sepbycommas pinit)
                List.zip
                |>> List.map (fun (v, i) -> {v with def = {v.def with init=i}})
            >>= toSet byName byName
        with | :? System.ArgumentException ->
            fail "Tuples must contain the same numbers of variables and initializers."
    let plstigkeys name =
        (sepbysemis (ptuple name) |> ws)
        |>> List.mapi (fun i -> Set.map (fun v ->{v with def={v.def with location = L(name, i)}}))

    (ws (skipString "stigmergy"))
    >>. (followedBy IDENTIFIER >>. getPosition) .>>. (ws IDENTIFIER)
    >>= (fun (pos, n) ->
            ((ws (pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS"))
            |> betweenBraces)
            |>> fun (l, v) -> {pos=pos; name=n; def={name=n; link=l; vars=v}}) <!> "STIGMERGY"