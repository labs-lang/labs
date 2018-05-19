module internal Expressions
open Types
open FParsec


/// Parser for values
let pval = choice [pint32 |>> Int; ppoint |>> Val.P]


let parithmop : Parser<_> =
    choice [
        (charReturn '+' Plus);
        (charReturn '-' Minus);
        (charReturn '*' Times);
        (charReturn '%' Mod)
    ]

// Example of a recursive parser
// http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
let pexpr, pexprRef = createParserForwardedToRef()

let pexprTerm = 
    choice [
        betweenParen pexpr;
        KEYNAME |>> K;
        pval |>> Const;
    ]


// assign to "pexprRef" the choice of the above parsers
do pexprRef :=
    maybeTuple2 (ws pexprTerm) 
        (ws parithmop .>>. pexpr)
        (fun (a,(b,c)) -> Arithm(a,b,c))

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
