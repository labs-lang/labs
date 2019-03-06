﻿namespace LabsCore
open Types

module Expr =
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
        | Leaf l -> fleaf l
        | Arithm(e1, op, e2) -> Arithm(recurse e1, op, recurse e2)
        | Ref r -> 
            let newOffset = r.offset |> Option.map recurse
            fref r newOffset
        | Unary(u, e) -> Unary(u, recurse e)

    
    let getVars expr =
        fold (fun a _ -> a) (fun a r -> Set.add r.var a) Set.empty expr
        
module BExpr = 
    let rec map fleaf fexpr bexpr =
        let recurse = map fleaf fexpr
        match bexpr with
        | BLeaf b -> fleaf b
        | Neg b -> Neg(recurse b)
        | Compound(b1, op, b2) -> Compound(recurse b1, op, recurse b2)
        | Compare(e1, op, e2) -> 
            Compare(fexpr e1, op, fexpr e2)
    let rec cata fleaf fneg fcompare fcompound bexpr = 
        let recurse = cata fleaf fneg fcompare fcompound
        match bexpr with
        | BLeaf b -> fleaf b 
        | Neg b -> fneg (recurse b)
        | Compare(e1, op, e2) -> fcompare op e1 e2
        | Compound(b1, op, b2) -> fcompound op (recurse b1) (recurse b2)