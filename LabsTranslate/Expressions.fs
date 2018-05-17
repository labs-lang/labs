module Expressions
open Types
open Base

let rec translateExpr (mapping:Map<(string * TypeofKey), int>) = function
| Const(Int(i)) -> sprintf "%i" i
//| Const(String(s)) -> "\"" + s + "\""
| Const(Val.P(p1,p2)) -> ""
| K(k) when mapping.ContainsKey (k, I)  -> 
    sprintf "comp[tid].I[%i]" mapping.[k,I]
| K(k) when mapping.ContainsKey (k, L)  -> 
    sprintf "comp[tid].Lvalue[%i]" mapping.[k,L]
| K(k) when mapping.ContainsKey (k, E)  -> 
    // TODO
    sprintf "E[%i]" mapping.[k,E]
| K(k) -> failwith "Unexpected key"
| Sum(e1, e2) -> 
    sprintf "( (%s) + (%s) )" 
        (translateExpr mapping e1) (translateExpr mapping e2)

let translateBOp = function
| Less -> "<"
| Equal -> "=="
| Greater -> ">"

let rec translateBExpr (mapping:Map<(string * TypeofKey), int>) = function
| True -> "true"
| False -> "false"
| Neg(b) -> sprintf "!(%s)" (translateBExpr mapping b)
| Conj(b1, b2) -> 
    sprintf "((%s) && (%s))" 
        (translateBExpr mapping b1) (translateBExpr mapping b2)
| Compare(e1, op, e2) ->
    sprintf "((%s) %s (%s))"
        (translateExpr mapping e1)
        (translateBOp op)
        (translateExpr mapping e2)


let rec getLstigKeys (mapping:Map<(string * TypeofKey), int>) = function
| K(k) when mapping.ContainsKey (k,L) -> (k,L) |> Set.singleton
| Sum(e1, e2) -> Set.union (getLstigKeys mapping e1) (getLstigKeys mapping e2)
| _ -> Set.empty

let lstigKeys (mapping:Map<(string * TypeofKey), int>) expr =
    expr |> getLstigKeys mapping |> Set.map (fun x -> mapping.[x])
