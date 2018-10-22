module internal Expressions
open Types
open FParsec

let parithmop : Parser<_> =
    choice [
        (notFollowedBy (skipString "++") >>. charReturn '+' Plus <!> "Plus");
        (notFollowedBy (skipString "->") >>. charReturn '-' Minus <!> "Minus");
        (charReturn '*' Times);
        (charReturn '/' Div);
        (charReturn '%' Mod)
    ]

let simpleRef p = 
    KEYNAME .>>. (opt (betweenBrackets p))
    |>> fun (str, offset) -> {var=str; offset=offset}

/// Creates a parser for arithmetic expressions, given
/// a parser for referencss "pref" and a parser for 
/// ids "pid".
let rec makeExprParser pref pid =
    let pexpr, pexprRef = createParserForwardedToRef()

    let pexprTerm = 
        choice [
            betweenParen pexpr <!> "paren"
            followedBy (skipString "abs(") >>.
                (skipString "abs") >>. betweenParen pexpr |>> Abs
            followedBy pint32 >>. pint32 |>> Const <!> "const"
            followedBy pid >>. pid |>> Id <!> "id"
            attempt (pref pexpr) |>> Ref <!> "ref"
        ]
    do pexprRef :=
        maybeTuple2 (ws pexprTerm <!> "term") 
            ((ws parithmop .>>. pexpr) <!> "arithm")
            (fun (a,(b,c)) -> Arithm(a,b,c))
    pexpr

/// Creates a parser for boolean expressions.
let makeBExprParser pexpr =
    let pbop : Parser<_> =
        choice [
            (stringReturn "and" Conj <!> "Conj");
            (stringReturn "or" Disj <!> "Disj");
        ]
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
        maybeTuple2 (ws pbexprTerm) ((ws pbop)  .>>. pbexpr)
            (fun (a,(b,c)) -> Compound(a,b,c))
    pbexpr