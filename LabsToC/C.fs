module LabsToC.C
open LabsCore
open Types
open Frontend
open Common

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let translateInitLocation a b c = (sprintf "_%s") (translateLocation a b c)

let private translate trRef trId =
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

let p name =
    let r = (trref translateLocation name)
    let agentExprTr, agentGuardTr =
        makeTranslators
            translate (BExprC (Some <| fun r -> (fst r.var).init = Undef))
            (trref translateLocation name) (fun () -> name) 
    
    let linkTr =
        let trLinkId = function | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
        translate (fun (v, cmp) offset -> trref translateLocation (trLinkId cmp) v offset) trLinkId
        |> BExprC (Some <| fun r -> ((fst << fst) r.var).init = Undef)
    
    let initTr n =
        BExprC None (translate (trref translateInitLocation n) (fun () -> n))

    let mainGuardTr =
        BExprC None (translate (trref translateInitLocation "firstAgent") (fun () -> "firstAgent"))
    
    let propTr =
        translateProp translate (BExprC (Some <| fun r -> ((fst << fst) r.var).init = Undef)) translateLocation
           
    {
        agentExprTr = agentExprTr
        agentGuardTr = agentGuardTr
        initTr = initTr
        linkTr = linkTr
        mainGuardTr = mainGuardTr
        propTr = propTr
    }