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
        (opt (betweenBrackets p |> ws))
        (choice [
            followedBy OF >>. ws OF >>. pOf
            preturn (None, None)
//            followedBy OF >>. (ws OF >>. (ws KEYNAME)) |>> Some
//            preturn None
        ])
        (fun k offset (ofVar, ofNum) -> {Var=(k, ofVar); Offset=offset; OfAgent=ofNum})

let pproperty withModality =
    let propertyLink = 
        (ws (skipString "id"))
        >>. (ws OF)
        >>.(ws KEYNAME)
    let pbaseprop = makeBExprParser (makeExprParser propertyRef propertyLink (fail "ifelse in properties not supported yet"))
    let pmodality = 
        choice [
            stringReturn "always" Always
            stringReturn "eventually" Eventually
            attempt (stringReturn "fairly_inf" FairlyInf)
            attempt (stringReturn "fairly" Fairly)
            stringReturn "finally" Finally
        ] |> ws
    pipe5
        (followedBy IDENTIFIER >>. getPosition)
        (ws IDENTIFIER .>> (ws EQ))
        (if withModality then pmodality else preturn Always)
        ((sepEndBy pquantifier (ws COMMA)) >>= toMap)
        pbaseprop
        (fun pos n m qs pred -> {Pos=pos; Name=n; Source=""; Def= {
            Name=n
            Modality=m
            Quantifiers=qs
            Predicate=pred
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