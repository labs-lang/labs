module LabsCore.Expr
open LabsCore.ExprTypes
open System

let rec internal foldB foldExpr acc bexpr =
    let recurse = foldB foldExpr
    match bexpr with
    | BLeaf _ -> acc
    | Compare (e1, _, e2) ->
        Seq.fold foldExpr acc [e1; e2]
    | Neg b -> recurse acc b 
    | Compound (_, bexprs) -> Seq.fold recurse acc bexprs

let rec fold fleaf fref acc expr = 
    let recurse = fold fleaf fref
    match expr with
    | QB (_, p)
    | Count (_, _, p) -> foldB recurse acc p
    | Leaf l -> fleaf acc l
    | Nondet(e1, e2, _)
    | Arithm(e1, _, e2) ->
        Seq.fold recurse acc [e1; e2]
    | Unary(_, e) -> recurse acc e
    | RawCall(_, args) -> Seq.fold recurse acc args
    | IfElse(_, e1, e2) ->
        Seq.fold recurse acc [e1; e2]
    | Ref r ->
        let newacc = fref acc r
        r.Offset 
        |> Option.map (List.fold recurse newacc)
        |> Option.defaultValue newacc

let rec cata fleaf farithm funary fnondet fref fraw fif expr = 
    let recurse = cata fleaf farithm funary fnondet fref fraw fif
    
    match expr with
    | QB _
    | Count _ -> failwith $"Unexpected call to Expr.cata on {expr}"
    | Leaf l -> fleaf l
    | Arithm(e1, op, e2) -> farithm op (recurse e1) (recurse e2)
    | Unary(op, e) -> funary op (recurse e)
    | Nondet(e1, e2, pos) -> fnondet (recurse e1) (recurse e2) pos
    | Ref r ->  
        fref r.Var (Option.map (List.map recurse) r.Offset) (Option.map recurse r.OfAgent)
    | RawCall(n, args) -> fraw n (List.map recurse args)
    | IfElse (cond, iftrue, iffalse) -> fif cond (recurse iftrue) (recurse iffalse)

let rec map_ fleaf fref fcond expr =
    let recurse = map_ fleaf fref fcond
    let rec mapP = function
        | BLeaf b -> BLeaf b
        | Compare (e1, op, e2) ->
            Compare (recurse e1, op, recurse e2)
        | Neg b -> Neg (mapP b)
        | Compound (bop, bexprs) -> Compound (bop, List.map mapP bexprs)
    
    match expr with
    | QB (q, p) -> QB(q, mapP p)
    | Count (typ, name, bexpr) -> Count (typ, name, mapP bexpr) 
    | Leaf l -> Leaf(fleaf l)
    | Nondet(e1, e2, pos) -> Nondet(recurse e1, recurse e2, pos)
    | Arithm(e1, op, e2) -> Arithm(recurse e1, op, recurse e2)
    | Ref r -> 
        let newOffset = r.Offset |> Option.map (List.map recurse)
        let newOf = r.OfAgent |> Option.map recurse
        Ref(fref r newOffset newOf)
    | RawCall (n, args) -> RawCall(n, List.map recurse args)
    | IfElse (cond, iftrue, iffalse) -> IfElse(fcond recurse cond, recurse iftrue, recurse iffalse)
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
    cata leafFn arithmFn unaryFn failure failure failure (fun _ -> failure) expr

/// <summary>
/// Evaluates a constant expression. Fails if the expression contains an 
/// <c>Id</c> object.
/// </summary>
/// <param name="expr">The constant expression.</param>
/// <seealso cref="evalConstExpr" />
/// <returns>The value of <c>expr</c>.</returns>
let evalCexprNoId expr = evalConstExpr (fun _ -> failwith "id not allowed here.") expr

let map fleaf fref = map_ fleaf fref (BExpr.map BLeaf)