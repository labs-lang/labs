module internal Expressions
open Types
open Tokens
open FParsec

let simpleRef p = 
    KEYNAME .>>. (opt (betweenBrackets p))
    |>> fun (str, offset) -> {var=str; offset=offset}


type ParseBExpr<'a, 'b> = 
    | E of Expr<'a, 'b>
    | B of BExpr<'a, 'b>

module ParseBExpr =
    let getB p =
        eprintfn "getB>>>>>>>> %A" p
        match p with
        | B b -> preturn b
        | E e -> fail (sprintf "%O is not a boolean expression" e)
    let getE p =
        match p with
        | B b -> fail (sprintf "%O is not an expression" b)
        | E e -> preturn e
    let compare op p1 p2 = 
        eprintfn ">>>>>>>> %O %O" p1 p2
        match p1, p2 with 
        | E e1, E e2 -> Compare(e1, op, e2) |> B
        | _ -> failwith "?"
    let arithm op p1 p2 = 
        eprintfn ">>>>>>>> %O %O" p1 p2
        match p1, p2 with 
        | E e1, E e2 -> Arithm(e1, op, e2) |> E
        | _ -> failwith "?"
    
    let compose op p1 p2 = 
        eprintfn ">>>>>>>> %O %O" p1 p2
        match p1, p2 with 
        | B b1, B b2 -> 
            Compound(b1, op, b2) |> B
        | _ -> failwith "?"
    let mapE f = function
        | E e -> E (f e)
        | B b -> B b
    let mapB f = function
        | E e -> E e 
        | B b -> B (f b)

/// Creates a parser for boolean expressions
let makeBExprParser pexpr =

    let opp = new OperatorPrecedenceParser<_, _, _>()
    let expr = opp.ExpressionParser
    
    let notInArrow = notFollowedBy (anyOf ['-'; '~']) >>. spaces
    
    let term : Parser<_> = 
        choice [
            safeStrReturn tTRUE (B True)
            safeStrReturn tFALSE (B False)
        ] <!> "bterm"

    let paren = 
        followedBy (skipChar '(')
        >>. choice [
            attempt (betweenParen pexpr) |>> E <!> "eparen"
            (betweenParen expr) <!> "bparen"
        ] |> ws

    opp.TermParser <- paren <|> (ws pexpr <!> "pexpr" |>> E) <|> (ws term)

    opp.AddOperator(InfixOperator(tCONJ, notInIdentifier, 1, Associativity.Left, ParseBExpr.compose Conj))
    opp.AddOperator(InfixOperator(tDISJ, notInIdentifier, 1, Associativity.Left, ParseBExpr.compose Disj))
    
    opp.AddOperator(InfixOperator("<", notInArrow, 2, Associativity.Left, ParseBExpr.compare Less))
    opp.AddOperator(InfixOperator(">", spaces, 2, Associativity.Left, ParseBExpr.compare Greater))
    opp.AddOperator(InfixOperator("=", spaces, 2, Associativity.Left, ParseBExpr.compare Equal))
    opp.AddOperator(InfixOperator("!=", spaces, 2, Associativity.Left, ParseBExpr.compare Neq))
    opp.AddOperator(InfixOperator("<=", spaces, 2, Associativity.Left, ParseBExpr.compare Leq))
    opp.AddOperator(InfixOperator(">=", spaces, 2, Associativity.Left, ParseBExpr.compare Geq))

    opp.AddOperator(PrefixOperator("!", spaces, 3, false, ParseBExpr.mapB Neg))

    expr >>= ParseBExpr.getB

let makeExprParser pref pid : Parser<_,_> =
    let pexpr, pexprRef = createParserForwardedToRef()
    let opp = new OperatorPrecedenceParser<Expr<'a,'b>,unit,unit>()
    let expr = opp.ExpressionParser

    let term =
        choice [
            followedBy pint32 >>. pint32 |>> Const <!> "const"
            followedBy pid >>. pid |>> Expr.Id <!> "id"
            attempt (pref expr) |>> Ref <!> "ref"
        ]

    opp.TermParser <- (ws term) <|> (ws (betweenParen expr) <!> "paren")

    // Same precedence rules as in C
    opp.AddOperator(InfixOperator(tPLUS, spaces, 1, Associativity.Left, fun x y -> Arithm(x, Plus, y)))
    opp.AddOperator(InfixOperator(tMINUS, notFollowedBy (skipChar '>') >>. spaces, 1, Associativity.Left, fun x y -> Arithm(x, Minus, y)))

    opp.AddOperator(InfixOperator(tMUL, spaces, 2, Associativity.Left, fun x y -> Arithm(x, Times, y)))   
    opp.AddOperator(InfixOperator(tDIV, spaces, 2, Associativity.Left, fun x y -> Arithm(x, Div, y)))   
    opp.AddOperator(InfixOperator(tMOD, spaces, 2, Associativity.Left, fun x y -> Arithm(x, Mod, y)))   

    opp.AddOperator(PrefixOperator(tABS, followedBy (skipChar '('), 3, false, fun x -> Unary(Abs, x)))
    opp.AddOperator(PrefixOperator(tMINUS, notFollowedBy (skipChar '>') >>. spaces, 3, false, fun x -> Unary(UnaryMinus, x)))

    expr

