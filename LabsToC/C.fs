module internal LabsToC.C
open LabsCore
open Types
open Common

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let translate trRef trId =
    let leaf_ = function
        | Id i -> trId i
        | Const i -> string i
        | Extern s -> s
    let arithm_ = function
        | Plus -> sprintf "(%s) + (%s)"
        | Minus -> sprintf "(%s) - (%s)"
        | Times -> sprintf "(%s) * (%s)"
        | Div -> sprintf "(%s) / (%s)"
        | Mod -> sprintf "mod(%s, %s)"
        | Max -> sprintf "__max(%s, %s)"
        | Min -> sprintf "__min(%s, %s)"
    let unary_ = function
        | UnaryMinus -> sprintf "-(%s)"
        | Abs -> sprintf "__abs(%s)"
    Expr.cata leaf_ arithm_ unary_ trRef

let rec private BExprC filter trExpr bexpr =
    let bleaf_ b = if b then "1" else "0"
    let neg_ = sprintf "!(%s)"
    let compound_ = function
        | Conj -> List.map (sprintf "(%s)") >> String.concat " & "
        | Disj -> List.map (sprintf "(%s)") >> String.concat " | "
    translateBExpr bleaf_ neg_ compound_ filter trExpr bexpr

 
let wrapper = { new Wrapper with
                member __.agentName = "tid"
                member __.initId n = Const n
                member __.trLinkId x = match x with | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
                member __.trBExpr filter trExpr b = BExprC filter trExpr b
                member __.trExpr trRef trId e = translate trRef trId e
                member __.trLoc loc x y = translateLocation loc x y
                member __.trInitLoc loc x y = (sprintf "_%s") (translateLocation loc x y)}