module internal Properties
open Types
open Expressions
open FParsec


let pprop, ppropRef = createParserForwardedToRef()

let propertyRef p =
    pipe2
        (simpleRef p)
        (ws (skipString "of") >>. (ws KEYNAME))
        (fun (a,b) c -> (a,c), b)

let prova1 = makeExprParser simpleRef
let prova = makeExprParser propertyRef

let pbaseprop = makeBExprParser (makeExprParser propertyRef)

do ppropRef :=
    let pQuantifier str pType = 
        tuple3
            (ws (skipString str) >>. (ws IDENTIFIER))
            ((ws KEYNAME) .>> ws (skipChar ','))
            pprop
        |>> pType

    let pAll = pQuantifier "forall" All
    let pSome = pQuantifier "exists" Exists
    ws (choice [pAll; pSome; pbaseprop |>> Prop])
    
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