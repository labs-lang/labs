module internal Init

open FParsec
open Types
open Common
open Expressions


let pconstexpr:Parser<Expr<unit,unit>> = 
    makeExprParser 
        (fun _ -> fail "unexpected variable in constant expression") 
        (skipString "id" >>. notFollowedBy (skipSatisfy isAlphanum))

let pvar loc = 
    pipe2 KEYNAME (opt (betweenBrackets puint32))
        (fun name -> 
            let v = {vartype=Scalar; name=name; location=loc; init=Undef}
            function
            | Some b -> {v with vartype=Array(int b)}
            | None -> v)

let pinit = 
    let pChoose = 
        (sepbycommas pconstexpr) |> betweenBrackets |>> Choose
    let pRange = 
        followedBy (pconstexpr >>. RANGE)
        >>. ((ws pconstexpr) .>>. (RANGE >>. (ws pconstexpr)) 
        |>> Range)
    let pSingle = (ws pconstexpr) |>> (Choose << List.singleton)
    let pUndef = stringReturn strUNDEF Undef

    choice [pChoose; pRange; pSingle; pUndef] |> ws

/// Parses a single init definition.
let pinitdef loc =
    (pvar loc) .>>. ((ws COLON) >>. pinit)
    |>> fun (var, init) -> {var with init=init}

let pkeys loc = 
    //let lbl = function I -> "interface" | L _ -> "stigmergy" | E -> "environment"
    ws (sepbysemis (ws (pinitdef loc)))
    >>= toSet byName byName 