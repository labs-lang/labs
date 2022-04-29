module LabsCore.ExprTypes
open Tokens
open FParsec


type ArithmOp =
    | Plus | Minus
    | Times | Div | RoundDiv | Mod
    | Min | Max
    override this.ToString() = 
        match this with
        | Plus -> tPLUS | Minus -> tMINUS
        | Times -> tMUL | Div -> tDIV | RoundDiv -> tROUNDDIV | Mod -> tMOD
        | Min -> tMIN | Max -> tMAX

type UnaryOp = 
    | Abs | UnaryMinus
    override this.ToString() =
        match this with
        | Abs -> tABS
        | UnaryMinus -> tMINUS


type Quantifier =
    | All
    | Exists
    override this.ToString() = match this with All -> "forall" | Exists -> "exists"

type CmpOp = 
    | Equal
    | Greater
    | Less
    | Leq
    | Geq
    | Neq
    override this.ToString() = 
        match this with
        | Less -> "<"
        | Equal -> "=="
        | Greater -> ">"
        | Leq -> "<="
        | Geq -> ">="
        | Neq -> "!="

type Bop =
    | Conj
    | Disj
    override this.ToString() = 
        match this with Conj -> tCONJ | Disj -> tDISJ

type LeafExpr<'b> =
    | Id of 'b
    | Const of int
    | Extern of string
    override this.ToString() = 
        match this with
        | Id _ -> tID
        | Const v -> string v 
        | Extern s -> "_" + s
type Expr<'a, 'b> =
    | QB of Map<string, string*Quantifier> * BExpr<'a, 'b>
    | Leaf of LeafExpr<'b>
    | Nondet of Expr<'a, 'b> * Expr<'a, 'b> * Position
    | Ref of Ref<'a, 'b>
    | Unary of UnaryOp * Expr<'a, 'b>
    | Arithm of Expr<'a, 'b> * ArithmOp * Expr<'a, 'b>
    | IfElse of Cond:BExpr<'a, 'b> * IfTrue:Expr<'a, 'b> * IfFalse:Expr<'a, 'b> 
    | RawCall of Name:string * Args:Expr<'a, 'b> list
    override this.ToString() = 
        match this with
        | QB (quants, pred) ->
            let qs = quants |> Map.values |> Seq.map string |> String.concat ", "
            $"{qs}, {string pred}"
        | Leaf l -> string l
        | Nondet (start, bound, _) -> $"[{start}..{bound}]"
        | Ref r -> string r
        | Unary(op, e) -> 
            let s = match op with Abs -> tABS | UnaryMinus -> tMINUS in $"%s{s}({e})"
        | RawCall (name, args) -> $"""@{name}({args |> List.map string |> String.concat ", "})"""
        | IfElse (cond, iftrue, iffalse) -> $"if ({cond}) then ({iftrue}) else ({iffalse})"
        | Arithm(e1, op, e2) ->
            match op with
            | Min | Max -> $"{op}({e1}, {e2})" 
            | _ -> $"{e1} {op} {e2}"
and Ref<'a, 'b> = 
    {Var:'a; Offset: Expr<'a, 'b> option; OfAgent: Expr<'a, 'b> option}
    override this.ToString() =
        let ofAgent = match this.OfAgent with None -> "" | Some e -> $" of {e}"
        let offset = match this.Offset with None -> "" | Some e -> $"[{e}]" 
        $"%O{this.Var}{offset}{ofAgent}"


///<summary>Boolean expressions.</summary>
and BExpr<'a, 'b> =
    | BLeaf of bool
    | Compare of Expr<'a, 'b> * CmpOp * Expr<'a, 'b>
    | Neg of BExpr<'a, 'b>
    | Compound of Bop * BExpr<'a, 'b> list
    override this.ToString() =
        match this with
        | BLeaf true -> tTRUE | BLeaf false -> tFALSE
        | Neg b -> $"%s{tNEG}({b})"
        | Compare(e1, op, e2) -> $"({e1}) {op} ({e2})"
        | Compound(op, b) -> List.map (sprintf "%O") b |> String.concat $" {op} "

/// Syntactic equality check.
let rec equal e1 e2 =
    match e1, e2 with
    | Leaf l1, Leaf l2 ->
        match l1, l2 with
        | Const x, Const y -> x = y
        | Id i, Id j -> i = j
        | Extern e1, Extern e2 -> e1 = e2
        | _ -> false
    | Arithm(e11, op1, e12), Arithm(e21, op2, e22) when op1 = op2 ->
        (equal e11 e21) && (equal e12 e22)
    | Unary(o1, e1_), Unary(o2, e2_) when o1 = o2 -> equal e1_ e2_
    | Ref r1, Ref r2 when r1.Var = r2.Var ->
        match r1.Offset, r2.Offset, r1.OfAgent, r2.OfAgent with
        | Some o1, Some o2, Some of1, Some of2 -> (equal o1 o2) && (equal of1 of2) 
        | None, None, None, None -> true
        | _ -> false
    | RawCall(n1, a1), RawCall(n2, a2) ->
        n1 = n2 &&
        a1.Length = a2.Length &&
        List.zip a1 a2 |> List.forall (fun (x1, x2) -> equal x1 x2)
    | _ -> false    
        
