module Frontend.Externs
open LabsCore.Expr
open LabsCore.BExpr
open LabsCore.Grammar
open Message

module ExprExterns =
    /// Replaces external parameters with their values.
    let replaceExterns (externs:Map<_,_>) expr =
        let leafFn = function
            | Id x -> Id x
            | Extern s ->
                match externs.TryFind s with
                | Some i -> Const i
                | None -> raise (LabsException {What=NoValueForExtern s; Where=[]})
            | Const x -> Const x
        LabsCore.Expr.map leafFn (fun r o of_ -> {r with Offset=o; OfAgent=of_}) expr
        
module BExprExterns =
    /// Replaces external parameters with their values.
    let replaceExterns externs = LabsCore.BExpr.map BLeaf (ExprExterns.replaceExterns externs)

module VarExterns =
    /// Replaces external parameters with their values.
    let replaceExterns externs v =
        let replace = ExprExterns.replaceExterns externs
        let init' =
            match v.Init with
            | Range(e1, e2) -> Range(replace e1, replace e2)
            | Choose(l) -> Choose(List.map replace l)
            | Undef -> Undef
        let vartype' =
            match v.Vartype with
            | Array e -> Array (replace e)
            | Scalar -> Scalar
        {v with Init=init'; Vartype=vartype'}

module ProcessExterns =
    /// Replaces external parameters with their values.
    let replaceExterns externs =
        let baseFn b =
            let doUpdate (r, expr) =
                {r with Offset=Option.map (ExprExterns.replaceExterns externs) r.Offset}, ExprExterns.replaceExterns externs expr
            match b.Def with
            | Act a -> BaseProcess {b with Def= Act {a with Updates = List.map doUpdate a.Updates}}
            | _ -> BaseProcess b
        LabsCore.Process.map baseFn (BExprExterns.replaceExterns externs)