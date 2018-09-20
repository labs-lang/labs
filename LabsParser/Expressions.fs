module internal Expressions
open Types
open FParsec

let parithmop : Parser<_> =
    choice [
        (charReturn '+' Plus <!> "Plus");
        (notFollowedBy (skipString "->") >>. charReturn '-' Minus <!> "Minus");
        (charReturn '*' Times);
        (charReturn '%' Mod)
    ]

let simpleRef p = 
    KEYNAME .>>. (opt (betweenBrackets p))
    |>> fun (str, offset) -> {var=str; offset=offset}

/// Creates a parser for arithmetic expressions.
let rec makeExprParser pref pid =
    let pexpr, pexprRef = createParserForwardedToRef()

    let pexprTerm = 
        choice [
            betweenParen pexpr <!> "paren"
            followedBy (skipString "abs(") >>.
                (skipString "abs") >>. betweenParen pexpr |>> Abs
            followedBy pint32 >>. pint32 |>> Const <!> "const"
            attempt (pref pexpr) |>> Ref <!> "ref"
            pid |>> Id <!> "id"
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
        let plessleq = skipChar '<' >>. ((charReturn '=' Leq) <|>% Less) 
        let pgreatgeq = skipChar '>' >>. ((charReturn '=' Geq) <|>% Greater)
        choice [
            (charReturn '=' Equal) <!> "Eq"
            (stringReturn "!=" Neq)
            plessleq <!> "Lessleq"
            pgreatgeq
        ]

    let pbexpr, pbexprRef = createParserForwardedToRef()
    let pbexprTerm : Parser<_> = 
        let pbexprNeg = NEG >>. pbexpr |>> Neg
        let pbexprCompare =
            tuple3 pexpr (ws pcompareop <!> "compare") pexpr |>> Compare
        choice [
            attempt pbexprNeg <!> "bneg"
            attempt pbexprCompare <!> "bcompare"
            betweenParen pbexpr <!> "bparen"
            stringReturn "true" True
            stringReturn "false" False
        ]
    do pbexprRef := 
        maybeTuple2 pbexprTerm ((ws CONJ)  >>. pbexpr) Conj
    pbexpr