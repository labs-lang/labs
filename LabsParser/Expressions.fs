module internal Expressions
open Types
open FParsec


/// Parser for values
let pval : Parser<_> = 
    let pintval = pint32 |>> Int
    let ppointval = ppoint |>> Val.P
    choice [pintval; ppointval]

// Example of a recursive parser
// http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
let pexpr, pexprRef = createParserForwardedToRef()

let rec pexprTerm = 
    choice [
        KEYNAME |>> K;
        pval |>> Const;
    ]

let private pexprSum = 
    maybeTuple2 (ws pexprTerm) ((ws (pchar '+')) >>. pexpr) Sum

// assign to "pexprRef" the choice of the above parsers
do pexprRef := pexprSum

/// Parser for comparison operators
let pcompareop : Parser<_> =
    choice [
        (charReturn '<' Less);
        (charReturn '=' Equal);
        (charReturn '>' Greater)
    ]

let pbexpr, private pbexprRef = createParserForwardedToRef()

let pbexprTerm : Parser<_> = 
    let pbexprNeg = NEG >>. pbexpr |>> Neg
    let pbexprCompare =
        tuple3 pexpr (ws pcompareop) pexpr |>> Compare
    choice [
        attempt pbexprNeg;
        attempt pbexprCompare;
        stringReturn "true" True;
        stringReturn "false" False;
    ]

let private pbexprConj =
    (ws pbexprTerm) .>>. ( (ws CONJ)  >>. pbexpr) |>> Conj

do pbexprRef := 
    choice [attempt pbexprConj; pbexprTerm]
