module internal Stigmergies

open FParsec
open LabsCore.ExprTypes
open LabsCore.Grammar
open LabsCore.Tokens
open Init
open Expressions


let plink =
    let pc1orc2 =
        ws (ws OF)
        >>. opt (skipChar 'c')
        >>. choice [ charReturn '1' C1; charReturn '2' C2 ]

    let linkref p =
        pipe3 (ws KEYNAME) (opt (ws (betweenBrackets (sepbycommas p)))) (opt pc1orc2) (fun a b c ->
            { Var = (a, c)
              Offset = b
              OfAgent = None })

    let linkId = ws (skipString tID) >>. pc1orc2

    getPosition
    .>>. makeBExprParser (makeExprParser linkref linkId (fail "ifelse in link predicates not supported yet"))
    |>> fun (pos, link) ->
        { Name = "link"
          Pos = pos
          Def = link
          Source = "" }

let plstig: Parser<_> =
    let plstigkeys name =
        let loc = L(name, 0)

        sepbysemis (sepbycommas (pvar loc)) |> ws
        |>> List.mapi (fun i -> List.map (fun v -> { v with Def.Location = L(name, i) }))
        |>> List.map Set.ofList

    ws (skipString "stigmergy") >>. (followedBy IDENTIFIER >>. getPosition)
    .>>. ws IDENTIFIER
    >>= (fun (pos, n) ->
        ws (pstringEq "link" plink) .>>. (plstigkeys n <!> "KEYS") |> betweenBraces
        |>> fun (l, v) ->
            { Pos = pos
              Name = n
              Def = { Name = n; Link = l; Vars = v }
              Source = "" })
    <!> "STIGMERGY"
