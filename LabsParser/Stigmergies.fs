module internal Stigmergies

open FParsec
open LabsCore.Expr
open LabsCore.BExpr
open LabsCore.Grammar
open LabsCore.Tokens
open Init
open Expressions


let plink =
    let pc1orc2 = 
        (ws (ws OF) >>. opt (skipChar 'c') >>. choice [charReturn '1' C1; charReturn '2' C2])
    let linkref p =
        pipe3
            (ws KEYNAME) (opt (betweenBrackets p)) (spaces >>. pc1orc2)
            (fun a b c -> {Var=a,c; Offset=b})
    let linkId = (ws (skipString tID)) >>. pc1orc2
    getPosition .>>. makeBExprParser (makeExprParser linkref linkId)
    |>> (fun (pos, link) -> {Name="link"; Pos=pos; Def=link; Source=""})

let plstig : Parser<_> =
    let ptuple name =
        let loc = L(name, 0)
        try
            pipe2
                (sepbycommas (pvar loc))
                ((ws COLON) >>. sepbycommas pinit)
                List.zip
                |>> List.map (fun (v, i) -> {v with Def = {v.Def with Init=i}})
            >>= toSet byName byName
        with | :? System.ArgumentException ->
            fail "Tuples must contain the same numbers of variables and initializers."
    let plstigkeys name =
        (sepbysemis (ptuple name) |> ws)
        |>> List.mapi (fun i -> Set.map (fun v ->{v with Def={v.Def with Location = L(name, i)}}))

    (ws (skipString "stigmergy"))
    >>. (followedBy IDENTIFIER >>. getPosition) .>>. (ws IDENTIFIER)
    >>= (fun (pos, n) ->
            ((ws (pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS"))
            |> betweenBraces)
            |>> fun (l, v) -> {Pos=pos; Name=n; Def={Name=n; Link=l; Vars=v}; Source=""}) <!> "STIGMERGY"