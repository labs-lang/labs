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
    getVars (fun (x, _) -> match x.location with L _ -> true | _ -> false) expr

let rec private translate trRef trId location =
    let translateAOp op e1 e2 = 
        match op with
        | Plus | Minus | Times | Div -> sprintf "( %s %O %s )" e1 op e2
        | Mod -> sprintf "mod( %s, %s )" e1 e2
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
let trref cmp (v:Var, i:int) offset =
    do refTypeCheck v offset
    let index =
        match offset with
        | None -> string i
        | Some off -> sprintf "%i + %s" i off
    (translateLocation v.location) cmp index

let checkUndef filter trref expr =
    (getVars filter expr)
    |> Seq.map (fun x -> trref x None)
    |> Seq.map (sprintf "(%s != undef_value)")
    |> fun s -> if Seq.isEmpty s then "" else String.concat " & " s

let rec private translateBExpr filter trref trExpr bexpr =
    let translateBOp = function
    | Conj -> sprintf "((%s) & (%s))"
    | Disj -> sprintf "((%s) | (%s))"

    let trB = translateBExpr filter trref trExpr

    match bexpr with
    | True -> "1"
    | False -> "0"
    | Neg b -> sprintf "(!(%s))" (trB b)
    | Compound(b1, op, b2) -> 
        (translateBOp op) (trB b1) (trB b2)
    | Compare(e1, op, e2) ->
        let undef1, undef2 = (checkUndef filter trref e1), (checkUndef filter trref e2)
        sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2)
        |> (if undef1 <> "" then sprintf "(%s) & (%s)" undef1 else id)
        |> (if undef2 <> "" then sprintf "(%s) & (%s)" undef2 else id)

type TranslateFactory<'a, 'b> when 'a:comparison and 'b:comparison = {
    refTranslator: 'a -> (string option) -> string
    idTranslator: 'b -> string
    filterUndef: 'a -> bool
}
with
    member this.BExprTranslator loc = translateBExpr this.filterUndef this.refTranslator (this.ExprTranslator loc)
    member this.ExprTranslator l = translate this.refTranslator this.idTranslator l

let customProcExpr name = {
    refTranslator= trref name
    idTranslator= fun () -> name
    filterUndef= fun (v, _) -> v.init = Undef
}

let procExpr = customProcExpr "tid"

let linkExpr = 
    let trLinkId = function | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
    let trLinkRef (v, cmp) (offset:string option) =
        trref (trLinkId cmp) v offset 
    {
        refTranslator= trLinkRef
        idTranslator= trLinkId
        filterUndef= fun ((v, (_:int)), _) -> v.init = Undef
    }
