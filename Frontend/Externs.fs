module Frontend.Externs
open LabsCore
open Message
open Grammar

module Expr =
    /// Replaces external parameters with their values.
    let replaceExterns (externs:Map<_,_>) expr =
        let leafFn = function
            | Id x -> Id x
            | Extern s ->
                match externs.TryFind s with
                | Some i -> Const i
                | None -> raise (LabsException {What=NoValueForExtern s; Where=[]})
            | Const x -> Const x
        Expr.map leafFn (fun r o -> {r with Offset=o}) expr
        
module BExpr =
    /// Replaces external parameters with their values.
    let replaceExterns externs = BExpr.map BLeaf (Expr.replaceExterns externs)

module Var =
    /// Replaces external parameters with their values.
    let replaceExterns externs v =
        let replace = Expr.replaceExterns externs
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

module Process =
    /// Replaces external parameters with their values.
    let replaceExterns externs =
        let baseFn b =
            let doUpdate (r, expr) =
                {r with Offset=Option.map (Expr.replaceExterns externs) r.Offset}, Expr.replaceExterns externs expr
            match b.Def with
            | Act a -> BaseProcess {b with Def= Act {a with Updates = List.map doUpdate a.Updates}}
            | _ -> BaseProcess b
        Process.map baseFn (BExpr.replaceExterns externs)