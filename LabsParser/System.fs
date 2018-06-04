module internal System
open FParsec
open Common
open Types
open Link
open Expressions
open Properties
open Processes

let pspawn = 
    ws ((ws IDENTIFIER) .>>. (ws COLON >>. pint32)) |> sepbycommas >>= toMap

let mapToLinkTerm prop = fun _ -> 
    match prop with
    | PropertyTerm.ConstTerm(v) -> Reply(T (ConstTerm v))
    | PropertyTerm.KeyRef(k,c) -> 
        match c with 
        | "c1" -> Reply(T (KeyRefC1 k))
        | "c2" -> Reply(T (KeyRefC2 k))
        | s -> 
            sprintf "Invalid link identifier %s" s 
            |> fun msg -> Reply(Error, ErrorMessageList(Message msg))

let plinkexpr, private plinkexprRef = createParserForwardedToRef()

let plinkterm = 
    let pterm = ppropTerm >>= mapToLinkTerm
    let pparen = betweenParen plinkexpr
    let pabs = (skipString "abs") >>. (betweenParen (ws plinkexpr)) |> ws |>> Abs
    choice [pabs; pparen; attempt pterm]


do plinkexprRef := 
    maybeTuple2 (ws plinkterm) ((ws parithmop) .>>. (ws plinkexpr)) 
        (fun (t1, (op, t2)) -> Arithm(t1, op, t2))


let plink, private plinkRef = createParserForwardedToRef()

do plinkRef :=
    let pneg = NEG >>. plink |>> Neg
    let pcompare =
        tuple3 plinkexpr (ws pcompareop) plinkexpr |>> Compare
    choice [
        followedBy (pstring "true") >>. stringReturn "true" True;
        attempt pneg;
        attempt pcompare
    ] |> ws

let pextern = ws ((sepbycommas (skipChar '_' >>. KEYNAME))) |>> Set.ofList

let psys = 
    ws (skipString "system")
    >>. 
        (tuple5
            (opt (pstringEq "extern" (ws (skipRestOfLine true))))
            (opt (pstringEq "environment" pkeys))
            (pstringEq "spawn" pspawn)
            (pstringEq "link" plink)
            (processes)
        |> betweenBraces)
        |> ws
