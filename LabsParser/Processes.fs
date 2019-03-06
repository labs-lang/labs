module internal Processes

open FParsec
open Types
open Tokens
open LabsCore
open Expressions    

let pexpr = makeExprParser simpleRef (skipString tID .>> notInIdentifier)

/// Parses elementary processes ("actions")
let paction =
    let parseArrow =
        skipChar '<' >>. choice [
            followedBy (skipString "--") >>. stringReturn "--" Location.E; 
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
    let doBase (stmt, pos) = {stmt=stmt; pos=pos} |> BaseProcess
    let compose a b = Comp(a, b)

    let pGuarded = 
        let pguard = makeBExprParser pexpr
        followedBy ((ws pguard) >>. (ws GUARD))
        >>. tuple3 (ws pguard) ((ws GUARD) >>. pproc) getPosition |>> Guard <!> "Guard"
    let pBase =
        let pNil = safeStrReturn "Nil" Nil .>>. getPosition |>> doBase
        let pSkip = safeStrReturn tSKIP Skip .>>. getPosition |>> doBase
        let pParen = followedBy (skipChar '(') >>. (betweenParen pproc)
        
        choice [
             attempt pGuarded <!> "Guarded"
             attempt pNil <!> "Nil"
             attempt pSkip <!> "Skip"
             (IDENTIFIER |>> Name) .>>. getPosition |>> doBase <!> "Name"
             (paction |>> Act) .>>. getPosition |>> doBase <!> "Action"
             pParen <!> "Paren"
         ] <!> "BASE"

    let pseq = 
        sepBy1 (ws pBase) (ws SEQ) |>> (compose Seq) <!> "SEQ"
    let pchoice = 
        sepBy (ws pseq) (ws CHOICE) |>> (compose Choice) <!> "CHOICE"

    sepBy (ws pchoice) (ws PAR) |>> (compose Par) <!> "PAR" 

let processes = 
    let pdef = (ws IDENTIFIER) .>>. ((ws EQ) >>. (ws pproc <!> "PPROC"))
    ws (many pdef) >>= toMap