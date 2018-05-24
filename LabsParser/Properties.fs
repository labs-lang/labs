module internal Properties
open Types
open Expressions
open FParsec

//let ppropTerm, ppropTermRef = createParserForwardedToRef()
let pprop, ppropRef = createParserForwardedToRef()

let ppropTerm =
    let pkeyref = 
        (ws KEYNAME) .>> ws (skipString "of") .>>. (ws KEYNAME) |>> KeyRef
    choice [
        pkeyref;
        pint32 |>> ConstTerm
    ]

let pbaseprop = 
    ppropTerm .>>. (ws EQ >>. ppropTerm) |>> Prop

let pQuantifier str pType = 
    tuple3
        (ws (skipString str) >>. (ws IDENTIFIER))
        ((ws KEYNAME) .>> ws (skipChar ','))
        pprop
    |>> pType
do ppropRef :=
    let pAll = pQuantifier "forall" All
    let pSome = pQuantifier "exists" Exists
    ws (choice [pAll; pSome; pbaseprop])


let ptemp : Parser<_> = 
    let ptemptype = 
        choice [
            ws (stringReturn "finally" Finally);
            ws (stringReturn "always" Always)]
    ptemptype .>>. (ws pprop) |>> (fun (x,y) -> x y)

let pproperties = 
    (ws IDENTIFIER .>>. (ws EQ >>. ptemp))
    |> many
    >>= toMap
    |> (>>.) spaces
    |> ws
    |> betweenBraces
    |> (>>.) (ws (skipString "check"))