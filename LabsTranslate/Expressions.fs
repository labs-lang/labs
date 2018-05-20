module Expressions
open Types
open Base
open Templates

let translateAOp = function
| Plus -> "+", "sumTuple"
| Minus -> "-", "minusTuple"
| Times -> "*", "timesTuple"
| Mod -> "%", "modTuple"


let rec inferType (types:Map<Key,Val>) expr =
    match expr with
    | Const(Int(_)) -> Int(0)
    | Const(P(_)) -> P(0,0)
    | K(k) -> types.[k]
    | Arithm(e1, op, e2) -> 
        match (inferType types e1),(inferType types e2) with
        | P(_), P(_) -> P(0,0)
        | Int(_), Int(_) -> Int(0)
        | _ -> failwith (sprintf "Incorrect operation: %s" (expr.ToString()))

let rec translateExpr types (mapping:KeyMapping) expr =
    let trexp = translateExpr types mapping
    match expr with
    | Const(Int(i)) -> sprintf "%i" i
    //| Const(String(s)) -> "\"" + s + "\""
    | Const(Val.P(x,y)) -> translatePoint x y 
    | K(k) -> translateKey mapping "tid" k
    | Arithm(e1, op, e2) -> 
        match (inferType types expr) with
        | Int(_) -> sprintf "( (%s) %s (%s) )" (trexp e1) (translateAOp op |> fst) (trexp e2)
        | P(_) -> sprintf "%s(%s, %s)" (translateAOp op |> snd) (trexp e1) (trexp e2) // TODO

let translateBOp = function
| Less -> "<"
| Equal -> "=="
| Greater -> ">"
| Leq -> "<="

let rec translateBExpr types (mapping:Map<(string * TypeofKey), int>) =
    let trbexp = translateBExpr types mapping
    function
    | True -> "true"
    | False -> "false"
    | Neg(b) -> sprintf "!(%s)" (trbexp b)
    | Conj(b1, b2) -> 
        sprintf "((%s) && (%s))" 
            (trbexp b1) (trbexp b2)
    | Compare(e1, op, e2) ->
        sprintf "((%s) %s (%s))"
            (translateExpr types mapping e1)
            (translateBOp op)
            (translateExpr types mapping e2)


let rec getLstigKeys (mapping:Map<(string * TypeofKey), int>) = function
| K(k) when mapping.ContainsKey (k,L) -> (k,L) |> Set.singleton
| Arithm(e1, _, e2) -> Set.union (getLstigKeys mapping e1) (getLstigKeys mapping e2)
| _ -> Set.empty

let lstigKeys (mapping:Map<(string * TypeofKey), int>) expr =
    expr |> getLstigKeys mapping |> Set.map (fun x -> mapping.[x])


