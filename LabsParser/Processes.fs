module Processes

open FParsec
open Types
open Expressions    

let pexpr = makeExprParser simpleRef (skipString "id" >>. notFollowedBy (skipSatisfy isAlphanum))

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


// We need a recursive parser for LAbS processes
// (see pGuarded and pParen)
let pproc, pprocRef = createParserForwardedToRef()

do pprocRef :=
    let pBase =
        let pguard = makeBExprParser pexpr
        let pNil = skipString "Nil" >>. getPosition |>> Nil
        let pSkip = skipString "Skip" >>. getPosition |>> Skip
        let pGuarded = tuple3 (ws pguard) ((ws GUARD) >>. pproc) getPosition |>> BASE.Guard
        let pParen = followedBy (skipChar '(') >>. betweenParen pproc .>>. getPosition |>> Paren
        //let stops = (skipString ";" <|> skipString "||" <|> skipString "++") 
        choice [
             attempt pGuarded <!> "Guarded"
             attempt pNil <!> "Nil"
             attempt pSkip <!> "Skip"
             IDENTIFIER .>>. getPosition |>> Name <!> "Name"
             paction .>>. getPosition |>> Act <!> "Action"
             pParen <!> "Paren"
         ] <!> "BASE"

    let pseq = sepBy1 (ws pBase) (ws TSEQ) <!> "SEQ"
    let pchoice = sepBy (ws pseq) (ws TCHOICE) <!> "CHOICE"
    sepBy (ws pchoice) (ws TPAR) <!> "PAR"

let processes = 
    let pdef = (ws IDENTIFIER) .>>. ((ws EQ) >>. (pproc <!> "PROC"))
    ws (many pdef) >>= toMap