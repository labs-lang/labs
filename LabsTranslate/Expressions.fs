module Expressions
open Types
open Base
open Templates

let translateAOp = function
| Plus -> "+", "sumTuple"
| Minus -> "-", "minusTuple"
| Times -> "*", "timesTuple"
| Mod -> "%", "modTuple"


let rec inferType (mapping:KeyMapping) expr =
    match expr with
    | Const(Int(_)) -> Int(0)
    | Const(P(_)) -> P(0,0)
    | K(k) -> getTypeOrFail mapping k
    | Arithm(e1, op, e2) -> 
        match (inferType mapping e1),(inferType mapping e2) with
        | P(_), P(_) -> P(0,0)
        | Int(_), Int(_) -> Int(0)
        | _ -> failwith (sprintf "Incorrect operation: %s" (expr.ToString()))

let rec translateExpr (mapping:KeyMapping) expr =
    let trexp = translateExpr mapping
    match expr with
    | Const(Int(i)) -> sprintf "%i" i
    //| Const(String(s)) -> "\"" + s + "\""
    | Const(Val.P(x,y)) -> translatePoint x y 
    | K(k) -> translateKey mapping "tid" k
    | Arithm(e1, op, e2) -> 
        match (inferType mapping expr) with
        | Int(_) -> sprintf "( (%s) %s (%s) )" (trexp e1) (translateAOp op |> fst) (trexp e2)
        | P(_) -> sprintf "%s(%s, %s)" (translateAOp op |> snd) (trexp e1) (trexp e2) // TODO

let translateBOp = function
| Less -> "<"
| Equal -> "=="
| Greater -> ">"
| Leq -> "<="

let rec translateBExpr (mapping:KeyMapping) =
    function
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


let rec getLstigKeys (mapping:KeyMapping) = 
    function
    | K(k) -> 
        let info = getInfoOrFail mapping k
        if info.location = L then Set.singleton info.index else Set.empty
    | Arithm(e1, _, e2) -> Set.union (getLstigKeys mapping e1) (getLstigKeys mapping e2)
    | Const(_) -> Set.empty


