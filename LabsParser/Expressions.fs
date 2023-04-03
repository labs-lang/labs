module internal Expressions

open LabsCore.Tokens
open LabsCore.ExprTypes
open LabsCore.BExpr
open FParsec

let simpleRef p =
    tuple3
        KEYNAME
        (opt (betweenBrackets (sepbycommas p)) |> ws)
        (choice [
            ((followedBy OF) >>. ws OF >>. p |>> Some)
            ((notFollowedBy OF) >>% None)
        ])
    |>> fun (str, offset, ofAgent) -> {Var=str; Offset=offset; OfAgent=ofAgent}

type ParseBExpr<'a, 'b> = 
    | E of Expr<'a, 'b>
    | B of BExpr<'a, 'b>

module ParseBExpr =
    let getB p =
        match p with
        | B b -> preturn (simplify b)
        | E e -> Compare(e, Neq, Leaf <| Const 0) |> preturn
    let getE p =
        match p with
        | B b -> fail $"{b} is not an arithmetic expression"
        | E e -> preturn e
    let compare op p1 p2 = 
        match p1, p2 with 
        | E e1, E e2 -> Compare(e1, op, e2) |> B
        | _ -> failwith "?"
    let arithm op p1 p2 = 
        match p1, p2 with 
        | E e1, E e2 -> Arithm(e1, op, e2) |> E
        | _ -> failwith "?"
    
    let compose op p1 p2 = 
        match p1, p2 with 
        | B b1, B b2 -> Compound(op, [b1; b2]) |> B
        | _ -> failwith "?"
    let mapE f = function
        | E e -> E (f e)
        | B b -> B b
    let mapB f = function
        | E e -> E e 
        | B b -> B (f b)

/// Creates a parser for boolean expressions
let makeBExprParser pexpr =

    let opp = OperatorPrecedenceParser<_, _, _>()
    let expr = opp.ExpressionParser
    
    let notInArrow = notFollowedBy (anyOf ['-'; '~']) |> ws
         
    let term : Parser<_> = 
        choice [
            safeStrReturn tTRUE (BLeaf true |> B)
            safeStrReturn tFALSE (BLeaf false |> B)
            pexpr |>> E
        ] <!> "bterm"

    opp.TermParser <- choice [
        attempt <| ws term
        attempt <| ws pexpr <!> "pexpr" |>> E
        ws (betweenParen expr) <!> "bparen"
    ]
    
    opp.AddOperator(InfixOperator(tCONJ, ws notInIdentifier, 1, Associativity.Left, ParseBExpr.compose Conj))
    opp.AddOperator(InfixOperator(tDISJ, ws notInIdentifier, 1, Associativity.Left, ParseBExpr.compose Disj))
    
    opp.AddOperator(InfixOperator("<", notInArrow, 2, Associativity.Left, ParseBExpr.compare Less))
    opp.AddOperator(InfixOperator(">", wsUnit, 2, Associativity.Left, ParseBExpr.compare Greater))
    opp.AddOperator(InfixOperator("=", wsUnit, 2, Associativity.Left, ParseBExpr.compare Equal))
    opp.AddOperator(InfixOperator("!=", wsUnit, 2, Associativity.Left, ParseBExpr.compare Neq))
    opp.AddOperator(InfixOperator("<=", wsUnit, 2, Associativity.Left, ParseBExpr.compare Leq))
    opp.AddOperator(InfixOperator(">=", wsUnit, 2, Associativity.Left, ParseBExpr.compare Geq))

    opp.AddOperator(PrefixOperator("!", wsUnit, 3, false, ParseBExpr.mapB Neg))

    expr >>= ParseBExpr.getB

let makeExprParser pref pid pbexpr : Parser<_> =
    let opp = OperatorPrecedenceParser<Expr<'a,'b>,unit,unit>()
    let expr = opp.ExpressionParser
    let arithm op x y = Arithm(x, op, y)
    
    let prawcall =
        let opts = IdentifierOptions()
        (RAWPREFIX >>. identifier opts) .>>.
        (betweenParen (sepBy expr (ws COMMA)))
        
    
    let term =
        choice [
            pextern |>> Extern |>> Leaf <!> "extern"
            followedBy pint32 >>. pint32 |>> Const |>> Leaf <!> "const"
            followedBy pid >>. pid |>> Id |>> Leaf <!> "id"
            followedBy RAWPREFIX >>. prawcall |>> RawCall <!> "rawCall"  
            followedBy (skipChar '[') >>. betweenBrackets (pipe3 getPosition expr (skipString ".." >>. expr) (fun p e1 e2 -> Nondet(e1, e2, p))) <!> "nondet"
            attempt (ws (pref expr)) |>> Ref <!> "ref"
        ]
    
    let pprefixbinary tok op =
        followedBy (safeSkip tok)
        >>.(ws (safeSkip tok))
        >>. betweenParen ( pipe2 expr ((ws COMMA) >>. expr) (arithm op))
    let pmax = pprefixbinary tMAX Max
    let pmin = pprefixbinary tMIN Min
    
    let pcond =
        followedBy (safeSkip tIF)
        >>. tuple3
                (ws (safeSkip tIF) >>. pbexpr) 
                (ws (safeSkip tTHEN) >>. expr)
                (ws (safeSkip tELSE) >>. expr)
        |>> IfElse <!> "if-then-else"
    
    opp.TermParser <- [
            term
            pmax
            pmin
            pcond
            (betweenParen expr) <!> "paren"
        ]
        |> List.map ws
        |> choice

    // Same precedence rules as in C
    opp.AddOperator(InfixOperator(tPLUS, notFollowedBy (skipChar '+') |> ws, 1, Associativity.Left, arithm Plus))
    opp.AddOperator(InfixOperator(tMINUS, notFollowedBy (skipChar '>') |> ws, 1, Associativity.Left, arithm Minus))

    opp.AddOperator(InfixOperator(tMUL, wsUnit, 2, Associativity.Left, arithm Times))   
    opp.AddOperator(InfixOperator(tDIV, wsUnit, 2, Associativity.Left, arithm Div))
    opp.AddOperator(InfixOperator(tROUNDDIV, notFollowedBy (skipChar '=') |> ws, 2, Associativity.Left, arithm RoundDiv))
    opp.AddOperator(InfixOperator(tMOD, wsUnit, 2, Associativity.Left, arithm Mod))   
    
    opp.AddOperator(PrefixOperator(tABS, followedBy (skipChar '('), 3, false, fun x -> Unary(Abs, x)))
    opp.AddOperator(PrefixOperator(tMINUS, notFollowedBy (skipChar '>') |> ws, 3, false, fun x -> Unary(UnaryMinus, x)))

    expr
