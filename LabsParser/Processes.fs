module Processes

open FParsec
open Types
open Expressions    

let pexpr = makeExprParser simpleRef

/// Parses elementary processes ("actions")
let paction =
    let parseArrow =
        skipChar '<' >>. choice [
            followedBy (pstring "--") >>. stringReturn "--" EnvWrite; 
            charReturn '-' AttrUpdate;
            charReturn '~' LStigUpdate
        ]
    (pipe3 
        (ws (simpleRef pexpr))
        (ws parseArrow) 
        (ws pexpr) 
        (fun (target, offset) action e -> action(target, offset, e)))

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pguard = makeBExprParser pexpr
    let pNil = stringReturn "Nil" Nil
    let pSkip = stringReturn "Skip" Skip
    let pGuarded = (ws pguard) .>>. (ws (skipString "->") >>. pproc)
    choice [
        attempt pNil; 
        attempt pSkip;
        followedBy pguard >>. pGuarded |>> Await;
        IDENTIFIER |>> Name; 
        paction |>> Base;
        betweenParen pproc
    ]

do pprocRef := 
    // Returns a Process type from the corresponding char
    let OP : Parser<_> = 
        choice [
            (charReturn '&' (^+));
            (charReturn '|' (^|));
            (charReturn ';' (^.));
        ]
    // Either returns a single term, or creates a choice/par/seq
    // from two processes
    maybeTuple2 (ws pprocTerm) ((ws OP) .>>. (ws pproc)) (fun (a, (b, c)) -> b a c)
    
let processes = 
    let pdef = (ws IDENTIFIER) .>>. ((ws EQ) >>. (ws pproc))
    ws (many pdef) >>= toMap
