module LabsCore.BExpr
open ExprTypes


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
        | Equal when (equal e1 e2) -> BLeaf true
        | Neq when (equal e1 e2) -> BLeaf false
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

