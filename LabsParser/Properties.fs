module internal Properties
open FParsec
open LabsCore.ExprTypes
open LabsCore.Grammar
open Expressions

//p:Parser<Expr<(string * string option),'a>,unit> -> Parser<Ref<(string * string option),'a>,unit>
let propertyRef p =
    let pOf =
        choice [
            ws KEYNAME |>> (fun k -> Some k, None)
            ws puint32 |>> fun n -> None, int n |> Const |> Leaf |> Some    
        ]

    pipe3
        (ws KEYNAME)
        (opt (betweenBrackets (sepbycommas p) |> ws))
        (choice [
            followedBy OF >>. ws OF >>. pOf
            preturn (None, None)
        ])
        (fun k offset (ofVar, ofNum) -> {Var=(k, ofVar); Offset=offset; OfAgent=ofNum})


let pquantpred =
    let propertyLink = 
        (ws (skipString "id"))
        >>. (ws OF)
        >>.(ws KEYNAME)
    let pbaseprop = makeBExprParser (makeExprParser propertyRef propertyLink (fail "ifelse in properties not supported yet"))
    pipe2
        ((sepEndBy pquantifier (ws COMMA)) >>= toMap)
        pbaseprop
        (fun qs pred -> {
            Quantifiers=qs
            Predicate=pred
        })

let pscope =
    let genericScopeParser kw1 kw2 scope =
        tuple2
            ((ws (skipString kw1)) >>. (pquantpred |> betweenParen |> ws))
            ((ws (skipString kw2)) >>. (pquantpred |> betweenParen |> ws))
        |>> scope
    
    choice [
        (genericScopeParser "between" "and" Between)
        (genericScopeParser "from" "until" FromUntil)
    ]

let pmode scope =
    choice [
        skipString "thereIs" >>% ThereIs scope
        skipString "always" >>% Globally scope
        (ws pquantpred) .>> (skipString "precedes") |>> fun prec -> Precedes (scope, prec)
    ] |> ws

let pproperty withModality =
    let pmodality = 
        choice [
            stringReturn "always" Always
            stringReturn "eventually" Eventually
            attempt (stringReturn "fairly_inf" FairlyInf)
            attempt (stringReturn "fairly" Fairly)
            stringReturn "finally" Finally
            (pscope .>> (ws COMMA)) >>= pmode
        ] |> ws
    
    pipe4
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER .>> (ws EQ))
        (if withModality then pmodality else preturn Always)
        pquantpred
        (fun pos n m qp -> {Pos=pos; Name=n; Source=""; Def= {
            Name=n
            Modality=m
            QuantPredicate = qp
        }})
    |> withSkippedString (fun s x -> {x with Source=s}) 

let pproperties : Parser<_> =
    wsUnit
    >>. pproperty true |> many
    |> ws
    |> betweenBraces
    |> (>>.) (ws (skipString "check"))
    
let passume =
    wsUnit
    >>. pproperty false |> many
    |> ws
    |> betweenBraces
    |> (>>.) (ws (skipString "assume"))