module Expressions
open Types
open Base
open Templates

let translateAOp = function
    | Plus -> sprintf "( (%s) + (%s) )"
    | Minus -> sprintf "( (%s) - (%s) )"
    | Times -> sprintf "( (%s) * (%s) )"
    | Mod -> sprintf "mod(%s, %s)"

let rec translateExpr (mapping:KeyMapping) expr =
    let trexp = translateExpr mapping
    match expr with
    | Const(i) -> sprintf "%i" i
    | K(k) -> translateKey mapping "tid" k
    | Arithm(e1, op, e2) -> 
        ((translateAOp op) (trexp e1) (trexp e2))

let translateBOp = function
    | Less -> "<"
    | Equal -> "=="
    | Greater -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | Neq -> "!="

let rec translateBExpr (mapping:KeyMapping) = function
    | True -> "1"
    | False -> "0"
    | Neg(b) -> sprintf "!(%s)" (translateBExpr mapping b)
    | Conj(b1, b2) -> 
        sprintf "((%s) && (%s))" 
            (translateBExpr mapping b1) (translateBExpr mapping b2)
    | Compare(e1, op, e2) ->
        sprintf "((%s) %s (%s))"
            (translateExpr mapping e1)
            (translateBOp op)
            (translateExpr mapping e2)

let rec getLstigKeys (mapping:KeyMapping) = function
    | K(k) -> 
        let info = getInfoOrFail mapping k
        if info.location = L then Set.singleton info.index else Set.empty
    | Arithm(e1, _, e2) -> Set.union (getLstigKeys mapping e1) (getLstigKeys mapping e2)
    | Const(_) -> Set.empty


