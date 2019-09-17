module Frontend.Externs
open LabsCore
open Message
open Types

module Expr =
    let replaceExterns (externs:Map<_,_>) expr =
        let leaf_ = function
            | Id x -> Id x
            | Extern s ->
                match externs.TryFind s with
                | Some i -> Const i
                | None -> raise (LabsException {what=NoValueForExtern s; where=[]})
            | Const x -> Const x
        Expr.map leaf_ (fun r o -> {r with offset=o}) expr
        
module BExpr =
    let replaceExterns externs = BExpr.map (BLeaf) (Expr.replaceExterns externs)

module Var =
    let replaceExterns externs v =
        let replace = Expr.replaceExterns externs
        let init' =
            match v.init with
            | Range(e1, e2) -> Range(replace e1, replace e2)
            | Choose(l) -> Choose(List.map replace l)
            | Undef -> Undef
        let vartype' =
            match v.vartype with
            | Array e -> Array (replace e)
            | Scalar -> Scalar
        {v with init=init'; vartype=vartype'}

module Process =
    open LabsCore.Process
    let replaceExterns externs =
        let base_ b =
            let doUpdate (ref_, expr) =
                {ref_ with offset=Option.map (Expr.replaceExterns externs) ref_.offset}, Expr.replaceExterns externs expr
            match b.def with
            | Act a -> BaseProcess {b with def= Act {a with updates = List.map doUpdate a.updates}}
            | _ -> BaseProcess b
        map base_ (BExpr.replaceExterns externs)
                