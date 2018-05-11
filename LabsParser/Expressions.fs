module internal Expressions
open Types
open FParsec

/// Parser for values
let pval : Parser<_> = 
    let pintval = pint32 |>> Int
    let pstringval = betweenQuotes |>> Val.String
    let ppointval =
        between (pchar '(') (pchar ')') 
            (pint32 .>>. (betweenspaces (pchar ',') >>. pint32))
            |>> P
    choice [pintval; pstringval; ppointval]

// Example of a recursive parser
// http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
let pexpr, pexprRef = createParserForwardedToRef()

let pexprTerm = 
    choice [
        lstigKey |>> Expr.L; 
        interfaceKey |>> Expr.I;
        pval |>> Const
    ]

let private pexprSum = 
    maybeTuple2 (pexprTerm .>> ws) ((pchar '+' .>> ws) >>. pexpr) Sum

// assign to "pexprRef" the choice of the above parsers
do pexprRef := pexprSum

/// Parser for comparison operators
let pbcompareop : Parser<_> =
    choice [
        (charReturn '<' Less);
        (charReturn '=' Equal);
        (charReturn '>' Greater)
    ]

let pbexpr, private pbexprRef = createParserForwardedToRef()

let pbexprTerm : Parser<_> = 
    let pbexprNeg = NEG >>. pbexpr |>> Neg
    let pbexprCompare =
        tuple3 pexpr (pbcompareop .>> ws) pexpr |>> Compare
    choice [
        attempt pbexprNeg;
        attempt pbexprCompare;
        stringReturn "true" True;
        stringReturn "false" False;
    ] .>> ws

let private pbexprConj =
    pbexprTerm .>>. ( CONJ >>. ws >>. pbexpr) |>> Conj

do pbexprRef := 
    choice [attempt pbexprConj; pbexprTerm]
