module internal LabsToC.Lnt
open Frontend
open LabsCore
open Types
open Common

let translateLocation loc n e =
    let name =
        match n with
        | "agent.id"
        | "NatToInt(Nat(agent.id))" -> "agent"
        | "firstAgent" -> "a"
        | "a1" | "a2" -> n
        | _ -> sprintf "agents[%s]" n 
    match loc with
    | I -> sprintf "%s.I[IntToNat(%O)]" 
    | L _ -> sprintf "%s.L[IntToNat(%O)].value"
    | E -> fun _ -> sprintf "E[IntToNat(%O)]"
    |> fun f -> f name e

let translateInitLocation loc n e = "x"

let private translateExpr trRef trId =
    let leaf_ = function
        | Id i -> sprintf "(%s of Int)" (trId i)
        | Const i -> sprintf "(%i of Int)" i
        | Extern s -> s (*THIS SHOULD NEVER MATCH *)
    let arithm_ = function
        | Plus -> sprintf "(%s) + (%s)"
        | Minus -> sprintf "(%s) - (%s)"
        | Times -> sprintf "(%s) * (%s)"
        | Div -> sprintf "(%s) div (%s)"
        | Mod -> sprintf "(%s) mod (%s)"
        | Max -> sprintf "max(%s, %s)"
        | Min -> sprintf "min(%s, %s)"
    let unary_ = function
        | UnaryMinus -> sprintf "-(%s)"
        | Abs -> sprintf "abs(%s)"
    Expr.cata leaf_ arithm_ unary_ trRef

let rec private BExprLnt filter trExpr bexpr =
    let bleaf_ b = if b then "true" else "false"
    let neg_ = sprintf "(not(%s))"
    let compare_ op e1 e2 = sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2)
    let compound_ = function
        | Conj -> List.map (sprintf "(%s)") >> String.concat " and "
        | Disj -> List.map (sprintf "(%s)") >> String.concat " or "
        
    translateBExpr bleaf_ neg_ compare_ compound_ filter trExpr bexpr
    
let wrapper = { new Wrapper with
                member __.templateInfo = {baseDir = "templates/lnt"; extension = "lnt"}
                member __.agentName = "NatToInt(Nat(agent.id))"
                member __.initId _ = Extern "NatToInt(Nat(a.id))"
                member __.trLinkId x = match x with | C1 -> "a1" | C2 -> "a2"
                member __.trBExpr filter trExpr b = BExprLnt filter trExpr b
                member __.trExpr trRef trId e = translateExpr trRef trId e
                member __.trLoc loc x y = translateLocation loc x y
                member __.trInitLoc loc x y = translateInitLocation loc x y }    