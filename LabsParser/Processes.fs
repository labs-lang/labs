module Processes

open FParsec
open Types
open Expressions    

let pexpr = makeExprParser simpleRef (skipString "id" |> ws)

/// Parses elementary processes ("actions")
let paction =
    let parseArrow =
        skipChar '<' >>. choice [
            followedBy (skipString "--") >>. stringReturn "--" E; 
            charReturn '-' I;
            charReturn '~' (L "")
        ]
    tuple3 
        (ws (simpleRef pexpr) |> sepbycommas)
        (ws parseArrow) 
        (ws pexpr |> sepbycommas)
    >>= (fun (refs, action, exprs) -> 
        try {actionType=action; updates=List.zip refs exprs} |> preturn with
        | :? System.ArgumentException -> 
            fail "A multiple assignment should contain the same number of variables and expressions.")

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pguard = makeBExprParser pexpr
    let pNil = stringReturn "Nil" Nil
    let pSkip = stringReturn "Skip" Skip
    let pGuarded = (ws pguard) .>>. ((ws GUARD) >>. pproc)
    choice [
        attempt pNil <!> "Nil"; 
        attempt pSkip <!> "Skip";
        attempt pGuarded <!> "Guarded" |>> Await;
        IDENTIFIER |>> Name; 
        paction |>> Base;
        betweenParen pproc
    ]

do pprocRef := 
    // Turns a syntactic operator into a process composition
    let OP : Parser<_> = 
        choice [
            (stringReturn "++" (^+));
            (stringReturn "||" (^|));
            (charReturn ';' (^.));
        ]
    maybeTuple2 
        (ws pprocTerm)
        ((ws OP) .>>. (ws pproc))
        (fun (a, (b, c)) -> b a c)
    
let processes = 
    let pdef = (ws IDENTIFIER) .>>. ((ws EQ) >>. (ws pproc <!> "PPROC"))
    ws (many pdef) >>= toMap
