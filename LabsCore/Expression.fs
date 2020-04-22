namespace LabsCore
open Types

module Expr =

    // Syntactic equality
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
        | Ref r1, Ref r2 when r1.var = r2.var ->
            match r1.offset, r2.offset with
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
            r.offset 
            |> Option.map (recurse newacc)
            |> Option.defaultValue newacc

    let rec cata fleaf farithm funary fref expr = 
        let recurse = cata fleaf farithm funary fref
        match expr with
        | Leaf l -> fleaf l
        | Arithm(e1, op, e2) -> farithm op (recurse e1) (recurse e2)
        | Unary(op, e) -> funary op (recurse e)
        | Ref r -> fref r.var (Option.map recurse r.offset)
    
    let rec map fleaf fref expr =
        let recurse = map fleaf fref
        match expr with
        | Leaf l -> Leaf(fleaf l)
        | Arithm(e1, op, e2) -> Arithm(recurse e1, op, recurse e2)
        | Ref r -> 
            let newOffset = r.offset |> Option.map recurse
            Ref(fref r newOffset)
        | Unary(u, e) -> Unary(u, recurse e)

    let getRefs expr = fold (fun a _ -> a) (fun a r -> Set.add r a) Set.empty expr
    
    let getVars expr = Set.map (fun r -> r.var) (getRefs expr)
        
    let evalConstExpr idfun expr =
        let leaf_ = function
            | Const i -> i
            | Id x -> idfun x
            | Extern a -> failwithf "Not a constexpr: %s" a (* THIS SHOULD NEVER MATCH *)
        let arithm_ = function
            | Plus -> (+)
            | Minus -> (-)
            | Times -> (*)
            | Div -> (/)
            | Mod -> (%)
            | Max -> max
            | Min -> min
        let unary_ = function
            | UnaryMinus -> fun x -> -x
            | Abs -> abs
        cata leaf_ arithm_ unary_ (fun _ _ -> failwith "Not a constexpr") expr
    
    let evalCexprNoId expr = evalConstExpr (fun _ -> failwith "id not allowed here.") expr
        
module BExpr = 
    /// Returns a "canonical" version of bexpr.
    /// It uses structural comparison on the inner expressions 
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

    let rec simplify bexpr =
        let compare_ op e1 e2 =
            match op with  
            | Equal when (Expr.equal e1 e2) -> BLeaf true
            | Neq when (Expr.equal e1 e2) -> BLeaf false
            | _ -> Compare(e1, op, e2)
        let compound_ op ls =
            let lsSimpl = List.map simplify ls
            // Flatten nested Compound nodes
            let sameOp, others = List.partition (function | Compound(o, _) when o=op -> true | _ -> false) lsSimpl 
            sameOp
            |> List.map (function Compound(_, l) -> l | _ -> [])
            |> List.concat
            |> List.append others
            // Remove duplicate predicates
            |> List.distinctBy (canonical)
            |> fun l -> if l.Length = 0 then BLeaf true else Compound(op, l)
            
        /// Propagates boolean constants across compound bexprs    
        let constPropagation bexpr =
            // (true | bexpr) -> true 
            // (false & bexpr) -> false
            // (false | bexpr) -> bexpr 
            // (true & bexpr) -> bexpr
            let compound_ op ls =
                match op with
                | Disj ->
                    if List.contains (BLeaf true) ls
                    then BLeaf true
                    else 
                        ls |> List.filter (function BLeaf false -> false | _ -> true)
                        |> fun l -> if l.Length = 0 then BLeaf true else  Compound(Disj, l)
                | Conj ->
                    if List.contains (BLeaf false) ls
                    then BLeaf false
                    else 
                        ls |> List.filter (function BLeaf true -> false | _ -> true)
                        |> fun l -> if l.Length = 0 then BLeaf true else Compound(Conj, l)
            cata BLeaf Neg (fun op e1 e2 -> Compare(e1, op, e2)) compound_ bexpr
            
        cata BLeaf Neg compare_ compound_ bexpr
        |> constPropagation
