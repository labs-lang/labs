module Checker.Externs
open LabsCore
open Message
open Types

module Expr =
    open LabsCore.Expr
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
    open LabsCore.BExpr
    let replaceExterns externs = BExpr.map (BLeaf) (Expr.replaceExterns externs)

module Process =
    open LabsCore.Process
    let replaceExterns externs =
        let base_ b =
            match b.stmt with
            | Act a -> BaseProcess {b with stmt= Act {a with updates = List.map (fun (x, e) -> x, Expr.replaceExterns externs e) a.updates}}
            | _ -> BaseProcess b
        map base_ (BExpr.replaceExterns externs)