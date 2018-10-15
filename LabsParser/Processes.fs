module Processes

open FParsec
open Types
open Expressions    

let pexpr = makeExprParser simpleRef (skipString "id" |> ws)

/// Parses elementary processes ("actions")
let paction =
    let parseArrow =
        skipChar '<' >>. choice [
            followedBy (skipString "--") >>. stringReturn "--" EnvWrite; 
            charReturn '-' AttrUpdate;
            charReturn '~' LStigUpdate
        ]
    (pipe3 
        (ws (simpleRef pexpr) |> sepbycommas)
        (ws parseArrow) 
        (ws pexpr |> sepbycommas)
        // TODO customize error if length of r != length of e
        // (need try/with System.ArgumentException)
        (fun r action e -> action(List.zip r e)))

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pguard = makeBExprParser pexpr
    let pNil = stringReturn "Nil" Nil
    let pSkip = stringReturn "Skip" Skip
    let pGuarded = (ws pguard) .>>. (ws (skipString "->") >>. pproc)
    choice [
        attempt pNil <!> "Nil"; 
        attempt pSkip <!> "Skip";
        attempt pGuarded <!> "Guarded" |>> Await;
        IDENTIFIER |>> Name; 
        paction |>> Base;
        betweenParen pproc
    ]

do pprocRef := 
    // Returns a Process type from the corresponding char
    let OP : Parser<_> = 
        choice [
            (stringReturn "++" (^+));
            (stringReturn "||" (^|));
            (charReturn ';' (^.));
        ]
    // Either returns a single term, or creates a choice/par/seq
    // from two processes
    maybeTuple2 
        (ws pprocTerm)
        ((ws OP) .>>. (ws pproc))
        (fun (a, (b, c)) -> b a c)
    
let processes = 
    let pdef = (ws IDENTIFIER) .>>. ((ws EQ) >>. (ws pproc <!> "PPROC"))
    ws (many pdef) >>= toMap
