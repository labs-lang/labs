module Link
open Types

type LinkTerm<'a> =
    | RefC1 of k:'a
    | RefC2 of k:'a

type Link<'a> = BExpr<LinkTerm<'a>>

type Stigmergy<'a> = {
    name:string
    vars:Map<Var,Init>
    link:Link<'a>
}