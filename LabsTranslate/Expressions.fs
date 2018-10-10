module internal Expressions
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

let rec translate trRef trId location =
    let translateAOp = function
        | Plus -> sprintf "( %s + %s )"
        | Minus -> sprintf "( %s - %s )"
        | Times -> sprintf "( %s * %s )"
        | Mod -> sprintf "mod( %s, %s )"
    function
    | Id i -> 
        try (trId i) with
        | :? System.Collections.Generic.KeyNotFoundException ->
            sprintf "%s: Undefined component %s" location (i.ToString())
            |> failwith 
    | Const i -> sprintf "%i" i
    | Abs e -> sprintf "abs(%s)" (translate trRef trId location e)
    | Ref r ->
        try
            (trRef r.var (r.offset |> Option.map (translate trRef trId location)))
        with
        | :? System.Collections.Generic.KeyNotFoundException ->
            sprintf "%s: Undefined name %s" location (string r)
            |> failwith
    | Arithm(e1, op, e2) -> 
        ((translateAOp op) (translate trRef trId location e1) (translate trRef trId location e2))

/// Translates a variable reference.
let trref (mapping:KeyMapping) cmp (v:Var) offset =
    do refTypeCheck v offset
    let index =
        let _, i = mapping.[v.name]
        match offset with
        | None -> i.ToString()
        | Some off -> sprintf "%i + %s" i off
    (translateLocation v.location) cmp index

let translateExpr mapping = translate (trref mapping "tid") (fun () -> "tid")

let rec translateBExpr trExpr =
    let translateBOp = function
    | Conj -> sprintf "((%s) && (%s))"
    | Disj -> sprintf "((%s) || (%s))"
    let translateCOp = function
        | Less -> "<"
        | Equal -> "=="
        | Greater -> ">"
        | Leq -> "<="
        | Geq -> ">="
        | Neq -> "!="
    function
    | True -> "1"
    | False -> "0"
    | Neg b -> sprintf "!(%s)" (translateBExpr trExpr b)
    | Compound(b1, op, b2) -> 
        (translateBOp op)
            (translateBExpr trExpr b1) (translateBExpr trExpr b2)
    | Compare(e1, op, e2) ->
        sprintf "(%s) %s (%s)"
            (trExpr e1)
            (translateCOp op)
            (trExpr e2)

let translateGuard mapping l = translateBExpr (translateExpr mapping l)

let translateLink mapping l = 
    let trLinkId = function
    | Id1 -> "__LABS_link1"
    | Id2 -> "__LABS_link2"
    let trLinkRef (l:LinkTerm<Var>) (offset:string option) =
        match l with
        | RefC1 v -> trref mapping "__LABS_link1" v offset 
        | RefC2 v -> trref mapping "__LABS_link2" v offset
    translateBExpr (translate trLinkRef trLinkId l)

/// Returns the set of all stigmergy variables accessed by the expression.
let rec getLstigVars = function
    | Id _
    | Const _ -> Set.empty
    | Abs e -> getLstigVars e
    | Ref r -> 
        match r.offset with
        | Some e -> getLstigVars e
        | None -> Set.empty
        |> match r.var.location with | L _ -> Set.add r.var | _ -> id
    | Arithm(e1, _, e2) -> Set.union (getLstigVars e1) (getLstigVars e2)



