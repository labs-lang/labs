module internal Properties
open FParsec
open LabsCore.Grammar
open Expressions

//p:Parser<Expr<(string * string option),'a>,unit> -> Parser<Ref<(string * string option),'a>,unit>
let propertyRef p =
    pipe3
        (ws KEYNAME)
        (opt (betweenBrackets p |> ws))
        (choice [
            followedBy OF >>. (ws OF >>. (ws KEYNAME)) |>> Some
            preturn None
        ])
        (fun k offset y -> {Var=(k, y); Offset=offset})

let pquantifier =
        pipe3
            (ws <| choice [
                stringReturn "forall" All;
                stringReturn "exists" Exists
            ])
            (ws IDENTIFIER)
            (ws KEYNAME)
            (fun a b c -> c, (b, a))

let pproperty withModality =
    let propertyLink = 
        (ws (skipString "id"))
        >>. (ws OF)
        >>.(ws KEYNAME)
    let pbaseprop = makeBExprParser (makeExprParser propertyRef propertyLink)
    let pmodality = 
        choice [
            stringReturn "finally" Finally
            stringReturn "always" Always
            stringReturn "fairly_inf" FairlyInf
            stringReturn "fairly" Fairly]
        |> ws
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

let pproperties =
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