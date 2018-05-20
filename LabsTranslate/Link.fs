module Link
open Types
open Base
open Templates
open Link
open Expressions

let rec inferTypeLink (types:Map<Key,Val>) = 
    function
    | T(ConstTerm (v)) -> inferType types (Const v)
    | T(KeyRefC1(k))
    | T(KeyRefC2(k)) -> types.[k]
    | Abs(e)
    | D2(e) -> inferTypeLink types e
    | Arithm(e1, op, e2) -> 
        match (inferTypeLink types e1),(inferTypeLink types e2) with
        | P(_), P(_) -> P(0,0)
        | Int(_), Int(_) -> Int(0)
        | _ -> failwith "Incorrect operation"

let rec translateLinkExpr types mapping expr = 
    let tlinkexp = translateLinkExpr types mapping
    match expr with
    | T(ConstTerm (Int i)) -> sprintf "%i" i
    | T(ConstTerm (P (x,y))) -> translatePoint x y 
    | T(KeyRefC1(k)) -> (translateKey mapping "comp1" k)
    | T(KeyRefC2(k)) -> (translateKey mapping "comp2" k)
    | Abs(e) -> 
        match (inferTypeLink types e) with
        | Int(_) -> sprintf "abs(%s)" (tlinkexp e)
        | _ -> sprintf "absTuple(%s)" (tlinkexp e) //TODO
    | D2(e) ->
        match (inferTypeLink types e) with
        | Int(_) -> sprintf "d2int(%s)" (tlinkexp e) // TODO
        | _ -> sprintf "d2Tuple(%s)" (tlinkexp e)
    | Arithm(e1, op, e2) ->
        match (inferTypeLink types expr) with
        | Int(_) -> sprintf "(%s) %s (%s)" (tlinkexp e1) (translateAOp op |> fst) (tlinkexp e2)
        | P(_) -> sprintf "%s(%s, %s)" (translateAOp op |> snd) (tlinkexp e1) (tlinkexp e2)

let rec translateLink types mapping expr = 
    let tlexpr = translateLinkExpr types mapping
    let tl = translateLink types mapping
    match expr with
    | True -> "true"
    | Compare(a, op, b) -> sprintf "(%s)\n%s\n(%s)" (tlexpr a) (translateBOp op) (tlexpr b) //TODO
    | Neg(a) -> sprintf "!(%s)" (tl a)
    | Conj(a, b) -> sprintf "(%s) && (%s)" (tl a) (tl b)

let encodeLink types mapping link =
    translateLink types mapping link
    |> sprintf "return %s;"
    |> (indent 4)
    |> cfunc "int" "link" "int comp1, int comp2"