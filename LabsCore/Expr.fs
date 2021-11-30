module LabsCore.Expr
open System
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
    | Leaf of LeafExpr<'b>
    | Nondet of Expr<'a, 'b> * Expr<'a, 'b> * Position
    | Ref of Ref<'a, 'b>
    | Unary of UnaryOp * Expr<'a, 'b>
    | Arithm of Expr<'a, 'b> * ArithmOp * Expr<'a, 'b>
    | RawCall of Name:string * Args:Expr<'a, 'b> list
    override this.ToString() = 
        match this with
        | Leaf l -> string l
        | Nondet (start, bound, _) -> $"[{start}..{bound}]"
        | Ref r -> string r
        | Unary(op, e) -> 
            let s = match op with Abs -> tABS | UnaryMinus -> tMINUS in $"%s{s}({e})"
        | RawCall (name, args) -> $"""@{name}({args |> List.map string |> String.concat ", "})"""
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
        
let rec fold fleaf fref acc expr = 
    let recurse = fold fleaf fref
    match expr with
    | Leaf l -> fleaf acc l
    | Nondet(e1, e2, _)
    | Arithm(e1, _, e2) ->
        Seq.fold recurse acc [e1; e2]
    | Unary(_, e) -> recurse acc e
    | RawCall(_, args) -> Seq.fold recurse acc args
    | Ref r ->
        let newacc = fref acc r
        r.Offset 
        |> Option.map (recurse newacc)
        |> Option.defaultValue newacc

let rec cata fleaf farithm funary fnondet fref fraw expr = 
    let recurse = cata fleaf farithm funary fnondet fref fraw
    match expr with
    | Leaf l -> fleaf l
    | Arithm(e1, op, e2) -> farithm op (recurse e1) (recurse e2)
    | Unary(op, e) -> funary op (recurse e)
    | Nondet(e1, e2, pos) -> fnondet (recurse e1) (recurse e2) pos
    | Ref r -> fref r.Var (Option.map recurse r.Offset) (Option.map recurse r.OfAgent)
    | RawCall(n, args) -> fraw n (List.map recurse args)

let rec map fleaf fref expr =
    let recurse = map fleaf fref
    match expr with
    | Leaf l -> Leaf(fleaf l)
    | Nondet(e1, e2, pos) -> Nondet(recurse e1, recurse e2, pos)
    | Arithm(e1, op, e2) -> Arithm(recurse e1, op, recurse e2)
    | Ref r -> 
        let newOffset = r.Offset |> Option.map recurse
        let newOf = r.OfAgent |> Option.map recurse
        Ref(fref r newOffset newOf)
    | RawCall (n, args) -> RawCall(n, List.map recurse args)
    | Unary(u, e) -> Unary(u, recurse e)

let getRefs expr = fold (fun a _ -> a) (fun a r -> Set.add r a) Set.empty expr

let getVars expr = Set.map (fun r -> r.Var) (getRefs expr)
    
/// <summary>
/// Evaluates a constant expression.
/// </summary>
/// <param name="idfun">A function to evaluate <c>Id</c> objects.</param>
/// <param name="expr">The constant expression.</param>
/// <returns>The value of <c>expr</c>.</returns>
let evalConstExpr idfun expr =
    let leafFn = function
        | Const i -> i
        | Id x -> idfun x
        | Extern a -> failwithf $"Not a constexpr: {a}" (* THIS SHOULD NEVER MATCH *)
    let arithmFn = function
        | Plus -> (+)
        | Minus -> (-)
        | Times -> (*)
        | Div -> (/)
        | RoundDiv -> fun x y ->
            let result = (x + (y-1)) / y
            if x = Int32.MaxValue then -result else result
        | Mod -> (%)
        | Max -> max
        | Min -> min
    let unaryFn = function
        | UnaryMinus -> (~-)
        | Abs -> abs
    let failure = (fun _ _ -> failwith "Not a constexpr")
    cata leafFn arithmFn unaryFn failure failure failure expr

/// <summary>
/// Evaluates a constant expression. Fails if the expression contains an 
/// <c>Id</c> object.
/// </summary>
/// <param name="expr">The constant expression.</param>
/// <seealso cref="evalConstExpr" />
/// <returns>The value of <c>expr</c>.</returns>
let evalCexprNoId expr = evalConstExpr (fun _ -> failwith "id not allowed here.") expr
