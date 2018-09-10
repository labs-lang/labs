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
    
let simpleRef p = KEYNAME .>>. (opt (betweenBrackets p))

/// Creates a parser for arithmetic expressions.
let rec makeExprParser pref =
    let pexpr, pexprRef = createParserForwardedToRef()

    let pexprTerm = 
        choice [
            betweenParen pexpr <!> "paren";
            followedBy pint32 >>. pint32 |>> Const <!> "const";
            pref pexpr |>> Ref <!> "ref" ;
        ]
    do pexprRef :=
        maybeTuple2 (ws pexprTerm <!> "term") 
            ((ws parithmop .>>. pexpr) <!> "arithm")
            (fun (a,(b,c)) -> Arithm(a,b,c))
    pexpr

/// Creates a parser for boolean expressions.
let makeBExprParser pexpr =
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

    let pbexpr, pbexprRef = createParserForwardedToRef()
    let pbexprTerm : Parser<_> = 
        let pbexprNeg = NEG >>. pbexpr |>> Neg
        let pbexprCompare =
            tuple3 pexpr (ws pcompareop) pexpr |>> Compare
        choice [
            attempt pbexprNeg <!> "bneg";
            attempt pbexprCompare <!> "bcompare";
            betweenParen pbexpr <!> "bparen";
            stringReturn "true" True;
            stringReturn "false" False;
        ]
    do pbexprRef := 
        maybeTuple2 pbexprTerm ((ws CONJ)  >>. pbexpr) Conj
    pbexpr