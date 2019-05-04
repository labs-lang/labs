module internal LabsToC.Expressions
open LabsCore
open Types

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let refTypeCheck v (offset:'a option) =
    let test, msg = 
        match v.vartype with
        | Scalar -> offset.IsSome, (sprintf "Scalar %s treated as Array")
        | Array _ -> offset.IsNone, (sprintf "Array %s treated as Scalar")
    if test then failwith (msg v.name) else ()

/// Returns the set of all stigmergy variables accessed by the expression.
let getLstigVars expr =
    Expr.getVars expr
    |> Set.filter (fun (v, _) -> match v.location with L _ -> true | _ -> false)

let private translate trRef trId =
    let leaf_ = function
        | Id i -> trId i
        | Const i -> string i
        | Extern s -> s (*THIS SHOULD NEVER MATCH *)
    let arithm_ = function
        | Plus -> sprintf "(%s) + (%s)"
        | Minus -> sprintf "(%s) - (%s)"
        | Times -> sprintf "(%s) * (%s)"
        | Div -> sprintf "(%s) / (%s)"
        | Mod -> sprintf "mod(%s, %s)"
        | Max -> sprintf "__max(%s, %s)"
        | Min -> sprintf "__min(%s, %s)"
    let unary_ = function
        | UnaryMinus -> sprintf "-(%s)"
        | Abs -> sprintf "__abs(%s)"
    Expr.cata leaf_ arithm_ unary_ trRef


/// Translates a variable reference.
let trref cmp (v:Var<int>, i:int) offset =
    do refTypeCheck v offset
    let index =
        match offset with
        | None -> string i
        | Some off -> sprintf "%i + %s" i off
    (translateLocation v.location) cmp index

let checkUndef filter trref expr =
    Expr.getVars expr
    |> Set.filter filter
    |> Seq.map (fun x -> trref x None)
    |> Seq.map (sprintf "(%s != undef_value)")
    |> fun s -> if Seq.isEmpty s then "" else String.concat " & " s

let rec private translateBExpr filter trref trExpr bexpr =
    let bleaf_ b = if b then "1" else "0"
    let neg_ = sprintf "(!(%s))"
    let compound_ = function
        | Conj -> sprintf "((%s) & (%s))"
        | Disj -> sprintf "((%s) | (%s))"
    let compare_ op e1 e2 = 
        let undef1, undef2 = (checkUndef filter trref e1), (checkUndef filter trref e2)
        sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2)
        |> (if undef1 <> "" then sprintf "(%s) & (%s)" undef1 else id)
        |> (if undef2 <> "" then sprintf "(%s) & (%s)" undef2 else id)
    BExpr.cata bleaf_ neg_ compare_ compound_ bexpr
    
type TranslateFactory<'a, 'b> when 'a:comparison and 'b:comparison = {
    refTranslator: 'a -> (string option) -> string
    idTranslator: 'b -> string
    filterUndef: 'a -> bool
}
with
    member this.BExprTranslator = translateBExpr this.filterUndef this.refTranslator this.ExprTranslator
    member this.ExprTranslator = translate this.refTranslator this.idTranslator

let customProcExpr name = {
    refTranslator= trref name
    idTranslator= fun () -> name
    filterUndef= fun (v, _) -> v.init = Undef
}

let constExpr name = {
    refTranslator= fun () _ -> ""
    idTranslator= fun () -> name
    filterUndef= fun () -> false
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
