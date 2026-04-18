module internal Init

open FParsec
open LabsCore.ExprTypes
open LabsCore.Grammar
open LabsCore.Tokens
open Common
open Expressions

let pconstexpr: Parser<Expr<unit, unit>> =
    makeExprParser
        (fun _ -> fail "unexpected variable in constant expression")
        (skipString tID >>. notInIdentifier)
        (fail "ifelse in constexpr is not supported yet")

let pvar loc =
    pipe3 (followedBy KEYNAME >>. getPosition) KEYNAME (opt (betweenBrackets (sepbycommas pconstexpr))) (fun pos name ->
        let v =
            { Vartype = Scalar
              Name = name
              Location = loc }

        function
        | Some e ->
            { Pos = pos
              Name = name
              Source = ""
              Def = { v with Vartype = Array(e) } }
        | None ->
            { Pos = pos
              Name = name
              Source = ""
              Def = v })

let pinit =
    let wspcexpr = ws pconstexpr
    let pChoose = sepbycommas pconstexpr |> betweenBrackets |>> Choose

    let pRange =
        followedBy (wspcexpr >>. RANGE)
        >>. (ws pconstexpr .>>. (ws RANGE >>. wspcexpr) |>> Range)

    let pSingle = wspcexpr |>> (Choose << List.singleton)
    let UNDEF = stringReturn tUNDEF Undef
    choice [ pChoose; pRange; pSingle; UNDEF ] |> ws

let pkeys loc = ws (sepbysemis (ws (pvar loc)))
