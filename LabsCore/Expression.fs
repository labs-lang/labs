namespace LabsCore
open Grammar

module Expr =

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
            match r1.Offset, r2.Offset with
            | Some o1, Some o2 -> equal o1 o2
            | None, None -> true
            | _ -> false
        | _ -> false    
            
    let rec fold fleaf fref acc expr = 
        let recurse = fold fleaf fref
        match expr with
        | Leaf l -> fleaf acc l
        | Arithm(e1, _, e2) ->
            Seq.fold recurse acc [e1; e2]
        | Unary(_, e) -> recurse acc e
        | Ref r ->
            let newacc = fref acc r
            r.Offset 
            |> Option.map (recurse newacc)
            |> Option.defaultValue newacc

    let rec cata fleaf farithm funary fref expr = 
        let recurse = cata fleaf farithm funary fref
        match expr with
        | Leaf l -> fleaf l
        | Arithm(e1, op, e2) -> farithm op (recurse e1) (recurse e2)
        | Unary(op, e) -> funary op (recurse e)
        | Ref r -> fref r.Var (Option.map recurse r.Offset)
    
    let rec map fleaf fref expr =
        let recurse = map fleaf fref
        match expr with
        | Leaf l -> Leaf(fleaf l)
        | Arithm(e1, op, e2) -> Arithm(recurse e1, op, recurse e2)
        | Ref r -> 
            let newOffset = r.Offset |> Option.map recurse
            Ref(fref r newOffset)
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
            | Extern a -> failwithf "Not a constexpr: %s" a (* THIS SHOULD NEVER MATCH *)
        let arithmFn = function
            | Plus -> (+)
            | Minus -> (-)
            | Times -> (*)
            | Div -> (/)
            | Mod -> (%)
            | Max -> max
            | Min -> min
        let unaryFn = function
            | UnaryMinus -> (~-)
            | Abs -> abs
        cata leafFn arithmFn unaryFn (fun _ _ -> failwith "Not a constexpr") expr
    
    /// <summary>
    /// Evaluates a constant expression. Fails if the expression contains an 
    /// <c>Id</c> object.
    /// </summary>
    /// <param name="expr">The constant expression.</param>
    /// <seealso cref="evalConstExpr" />
    /// <returns>The value of <c>expr</c>.</returns>
    let evalCexprNoId expr = evalConstExpr (fun _ -> failwith "id not allowed here.") expr
        

module BExpr = 
    /// Returns a "canonical" version of bexpr.
    /// It uses structural comparison on the inner expressions.
    let rec canonical bexpr =
        match bexpr with
        | Compare(e1, Equal, e2) when e1 > e2 -> Compare(e2, Equal, e1)
        | Compare(e1, Neq, e2) when e1 > e2 -> Compare(e2, Neq, e1)
        | Compound(op, l) -> Compound(op, l |> List.map canonical |> List.sort)
        // TODO add other cases
        | _ -> bexpr
    
    let rec map fleaf fexpr bexpr =
        let recurse = map fleaf fexpr
        match bexpr with
        | BLeaf b -> fleaf b
        | Neg b -> Neg(recurse b)
        | Compound(op, b) -> Compound(op, List.map recurse b)
        | Compare(e1, op, e2) -> 
            Compare(fexpr e1, op, fexpr e2)
    let rec cata fleaf fneg fcompare fcompound bexpr = 
        let recurse = cata fleaf fneg fcompare fcompound
        match bexpr with
        | BLeaf b -> fleaf b 
        | Neg b -> fneg (recurse b)
        | Compare(e1, op, e2) -> fcompare op e1 e2
        | Compound(op, b) -> fcompound op (List.map recurse b)

    /// Turns a Boolean expression into a simpler, equivalent one.
    let rec simplify bexpr =
        let compareFn op e1 e2 =
            match op with  
            | Equal when (Expr.equal e1 e2) -> BLeaf true
            | Neq when (Expr.equal e1 e2) -> BLeaf false
            | _ -> Compare(e1, op, e2)
        let compoundFn op ls =
            let lsSimpl = List.map simplify ls
            // Flatten nested Compound nodes
            // e.g. (Conj b1 (Conj b2 b3)) becomes (Conj b1 b2 b3)
            let sameOp, others = List.partition (function | Compound(o, _) when o=op -> true | _ -> false) lsSimpl 
            sameOp
            |> List.collect (function Compound(_, l) -> l | _ -> [])
            |> List.append others
            |> List.distinctBy canonical // Remove duplicate predicates
            |> fun l -> if l.IsEmpty then BLeaf true else Compound(op, l)
            
        /// Propagates boolean constants within bexpr. 
        let constPropagation bexpr =
            let isTrue = function BLeaf true -> true | _ -> false
            let isFalse = function BLeaf false -> true | _ -> false

            let compoundFn op ls =
                match op with
                | Disj ->
                    // (true | bexpr) -> true 
                    if List.exists isTrue ls
                    then BLeaf true
                    else
                        // (false | bexpr) -> bexpr 
                        let ls1 = List.filter (not << isFalse) ls
                        if ls1.IsEmpty then BLeaf false else  Compound(Disj, ls1)
                | Conj ->
                    // (false & bexpr) -> false
                    if List.exists isFalse ls
                    then BLeaf false
                    else 
                        // (true & bexpr) -> bexpr
                        List.filter (not << isTrue) ls 
                        |> fun l -> if l.IsEmpty then BLeaf true else Compound(Conj, l)
            cata BLeaf Neg (fun op e1 e2 -> Compare(e1, op, e2)) compoundFn bexpr
            
        cata BLeaf Neg compareFn compoundFn bexpr
        |> constPropagation
