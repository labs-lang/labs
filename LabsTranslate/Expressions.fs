module Expressions
open Types
open Base
open Link
open Templates

let refTypeCheck v (offset:'a option) =
    let test, msg = 
        match v.vartype with
        | Scalar -> offset.IsSome, (sprintf "Scalar %s treated as Array")
        | Array(_) -> offset.IsNone, (sprintf "Array %s treated as Scalar")
    if test then failwith (msg v.name) else ()

let rec translate trRef =
    let translateAOp = function
        | Plus -> sprintf "( %s + %s )"
        | Minus -> sprintf "( %s - %s )"
        | Times -> sprintf "( %s * %s )"
        | Mod -> sprintf "mod( %s, %s )"
    function
    | Const i -> sprintf "%i" i
    | Ref r ->  
        //do refTypeCheck r
        (trRef r.var (r.offset |> Option.map (translate trRef)))
    | Arithm(e1, op, e2) -> 
        ((translateAOp op) (translate trRef e1) (translate trRef e2))

/// Translates a variable reference.
let trref (mapping:KeyMapping) cmp (v:Var) offset =
    do refTypeCheck v offset
    let index =
        let _, i = mapping.[v.name]
        match offset with
        | None -> i.ToString()
        | Some off -> sprintf "%i + %s" i off
    (translateLocation v.location) cmp index

let translateExpr mapping = translate (trref mapping "tid")

let rec translateBExpr trExpr =
    let translateBOp = function
        | Less -> "<"
        | Equal -> "=="
        | Greater -> ">"
        | Leq -> "<="
        | Geq -> ">="
        | Neq -> "!="
    function
    | True -> "1"
    | False -> "0"
    | Neg(b) -> sprintf "!(%s)" (translateBExpr trExpr b)
    | Conj(b1, b2) -> 
        sprintf "((%s) && (%s))" 
            (translateBExpr trExpr b1) (translateBExpr trExpr b2)
    | Compare(e1, op, e2) ->
        sprintf "(%s) %s (%s)"
            (trExpr e1)
            (translateBOp op)
            (trExpr e2)

let translateGuard mapping = translateBExpr (translateExpr mapping)

let translateLink mapping = 
    let trLinkRef (l:LinkTerm<Var>) (offset:string option) =
        match l with
        | RefC1 v -> trref mapping "__LABS_link1" v offset 
        | RefC2 v -> trref mapping "__LABS_link2" v offset
    translateBExpr (translate trLinkRef)

/// Returns the set of all stigmergy variables accessed by the expression.
let rec getLstigVars = function
    | Ref r -> 
        match r.offset with
        | Some e -> getLstigVars e
        | None -> Set.empty
        |> if r.var.location = L then Set.add r.var else id
    | Arithm(e1, _, e2) -> Set.union (getLstigVars e1) (getLstigVars e2)
    | Const(_) -> Set.empty


