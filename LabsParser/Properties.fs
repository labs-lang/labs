﻿module internal Properties
open FParsec
open Types
open Expressions

let propertyRef p =
    pipe3
        (ws KEYNAME)
        (opt (betweenBrackets p))
        (choice [
            followedBy OF >>. (ws OF >>. (ws KEYNAME)) |>> Some
            preturn None
        ])
        (fun k offset y -> {var=(k, y); offset=offset})

let pquantifier =
        pipe3
            (ws <| choice [
                stringReturn "forall" All;
                stringReturn "exists" Exists
            ])
            (ws IDENTIFIER)
            (ws KEYNAME)
            (fun a b c -> c, (b, a))

let pproperty = 
    let propertyLink = 
        (ws (skipString "id"))
        >>. (ws OF)
        >>.(ws KEYNAME)

    let pbaseprop = makeBExprParser (makeExprParser propertyRef propertyLink)
    let pmodality = 
        choice [
            stringReturn "finally" Finally;
            stringReturn "always" Always]
        |> ws
    pipe5
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER .>> (ws EQ))
        pmodality
        ((sepEndBy pquantifier (ws COMMA)) >>= toMap)
        pbaseprop
        (fun pos n m qs pred -> {pos=pos; name=n; def= {
            name=n
            modality=m
            quantifiers=qs
            predicate=pred
        }})

let pproperties =
    ws_
    >>. pproperty |> many
    |> ws
    |> betweenBraces
    |> (>>.) (ws (skipString "check"))