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

let rec getVars filter = function
    | Id _
    | Const _ -> Set.empty
    | Abs e -> getVars filter e
    | Ref r -> 
        match r.offset with
        | Some e -> getVars filter e
        | None -> Set.empty
        |> if (filter r.var) then Set.add r.var else id
    | Arithm(e1, _, e2) -> Set.union (getVars filter e1) (getVars filter e2)

/// Returns the set of all stigmergy variables accessed by the expression.
let rec getLstigVars expr =
    getVars (fun x -> match x.location with L _ -> true | _ -> false) expr


let rec translate trRef trId location =
    let translateAOp = function
        | Plus -> sprintf "( %s + %s )"
        | Minus -> sprintf "( %s - %s )"
        | Times -> sprintf "( %s * %s )"
        | Div -> sprintf "( %s / %s )"
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

let checkUndef filter trref expr =
    (getVars filter expr)
    |> Seq.map trref
    |> Seq.map (sprintf "(%O != undef_value)")
    |> String.concat " && "


let rec translateBExpr filter trref trExpr =
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
    let trB = translateBExpr filter trref trExpr
    function
    | True -> "1"
    | False -> "0"
    | Neg b -> sprintf "!(%s)" (trB b)
    | Compound(b1, op, b2) -> 
        (translateBOp op)
            (trB b1) (trB b2)
    | Compare(e1, op, e2) ->
        sprintf "(%s) %s (%s) && %s && %s"
            (trExpr e1)
            (translateCOp op)
            (trExpr e2)
            (checkUndef filter trref e1)
            (checkUndef filter trref e2)

let translateGuard mapping l = translateBExpr (translateExpr mapping l)

let translateLink mapping l = 
    let trLinkId = function
    | Id1 -> "__LABS_link1"
    | Id2 -> "__LABS_link2"
    let trLinkRef (v, cmp) (offset:string option) =
        match cmp with
        | C1 -> "__LABS_link1"
        | C2 -> "__LABS_link2"
        |> fun name -> trref mapping name v offset 
    translateBExpr (translate trLinkRef trLinkId l)


let prova mapping e = checkUndef (fun v -> v.init = Undef) (trref mapping "") e
let prova2 mapping e = 
    let trLinkRef (v, cmp) (offset:string option) =
            match cmp with
            | C1 -> "__LABS_link1"
            | C2 -> "__LABS_link2"
            |> fun name -> trref mapping name v offset 
    checkUndef (fun (v, _) -> v.init = Undef) trLinkRef e
    
type Prova<'a, 'b when 'a:comparison> = {
    refTranslator: 'a -> (string option) -> string
    idTranslator: 'b -> string
    filterUndef: 'a -> bool
}
with
    member this.BExprTranslator loc = translateBExpr (this.ExprTranslator loc)
    member this.ExprTranslator l = translate this.refTranslator this.idTranslator l
