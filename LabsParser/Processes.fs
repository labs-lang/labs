module Processes

open FParsec
open Types
open Expressions    


/// Parses elementary processes ("actions")
let paction : Parser<_> =
    let parseArrow : Parser<_> =
        choice [
            charReturn '-' AttrUpdate;
            charReturn '~' LStigUpdate;
            charReturn '=' EnvWrite
        ]
    (pipe3 
        (ws KEYNAME) 
        (ws parseArrow) 
        (ws pexpr) 
        (fun k action e -> action(k, e)))

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pNil = stringReturn "Nil" Nil
    let pSkip = stringReturn "Skip" Skip
    let pParen = between (pchar '(') (pchar ')') pproc
    choice [
        attempt pNil; 
        attempt pSkip; 
        IDENTIFIER |>> Process.Name; 
        paction |>> Base; 
        pParen
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