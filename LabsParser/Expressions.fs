module internal Expressions
open Types
open FParsec

let parithmop : Parser<_> =
    choice [
        (charReturn '+' Plus);
        (charReturn '-' Minus);
        (charReturn '*' Times);
        (charReturn '%' Mod)
    ]
        
//let ppropertyref =
    //(ws pvarref) .>> ws (skipString "of") .>>. (ws KEYNAME)
let simpleRef p = KEYNAME .>>. (opt (betweenBrackets p))

let rec makeExprParser pref =
    let pexpr, pexprRef = createParserForwardedToRef()

    let pexprTerm = 
        choice [
            followedBy pint32 >>. pint32 |>> Const;
            betweenParen pexpr;
            pref pexpr |>> Ref;
        ]
    do pexprRef :=
        maybeTuple2 (ws pexprTerm) 
            (ws parithmop .>>. pexpr)
            (fun (a,(b,c)) -> Arithm(a,b,c))
    pexpr



/// Parser for comparison operators
let pcompareop : Parser<_> =
    let plessleq = (attempt (stringReturn "<=" Leq)) <|> (charReturn '<' Less);
    let pgreatgeq = (attempt (stringReturn ">=" Geq)) <|> (charReturn '>' Greater);
    choice [
        plessleq;
        pgreatgeq;
        (stringReturn "!=" Neq);
        (charReturn '=' Equal);
    ]

let makeBExprParser pexpr =
    let pbexpr, pbexprRef = createParserForwardedToRef()
    let pbexprTerm : Parser<_> = 
        let pbexprNeg = NEG >>. pbexpr |>> Neg
        let pbexprCompare =
            tuple3 pexpr (ws pcompareop) pexpr |>> Compare
        choice [
            betweenParen pbexpr;
            attempt pbexprNeg;
            attempt pbexprCompare;
            stringReturn "true" True;
            stringReturn "false" False;
        ]
    do pbexprRef := 
        maybeTuple2 pbexprTerm ((ws CONJ)  >>. pbexpr) Conj
    pbexpr

let pexpr = makeExprParser simpleRef
let pbexpr = makeBExprParser pexpr