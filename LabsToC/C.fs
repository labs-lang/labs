module internal LabsToC.C
open LabsCore
open LabsCore.Grammar
open Common

let private translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let private translate trRef trId =
    let leafFn = function
        | Id i -> trId i
        | Const i -> string i
        | Extern s -> s
    let arithmFn = function
        | Plus -> sprintf "(%s) + (%s)"
        | Minus -> sprintf "(%s) - (%s)"
        | Times -> sprintf "(%s) * (%s)"
        | Div -> sprintf "(%s) / (%s)"
        | Mod -> sprintf "mod(%s, %s)"
        | Max -> sprintf "__max(%s, %s)"
        | Min -> sprintf "__min(%s, %s)"
    let unaryFn = function
        | UnaryMinus -> sprintf "-(%s)"
        | Abs -> sprintf "__abs(%s)"
    Expr.cata leafFn arithmFn unaryFn trRef

let rec private trBExprC filter trExpr bexpr =
    let bleafFn b = if b then "1" else "0"
    let negFn = sprintf "!(%s)"
    let compareFn op e1 e2 = sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2) //TODO
    let compoundFn = function
        | Conj -> List.map (sprintf "(%s)") >> String.concat " & "
        | Disj -> List.map (sprintf "(%s)") >> String.concat " | "
    translateBExpr bleafFn negFn compareFn compoundFn filter bexpr

 
let wrapper = { new IWrapper with
                member __.TemplateInfo = {BaseDir = "templates"; Extension = "c"}
                member __.AgentName = "tid"
                member __.InitId n = Const n
                member __.TrLinkId x = match x with | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
                member __.TrBExpr filter trExpr b = trBExprC filter trExpr b
                member __.TrExpr trRef trId e = translate trRef trId e
                member __.TrLoc loc x y = translateLocation loc x y
                member __.TrInitLoc loc x y = (sprintf "_%s") (translateLocation loc x y)}