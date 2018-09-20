module Link
open Types

type LinkTerm<'a> =
    | RefC1 of k:'a
    | RefC2 of k:'a

type LinkId = Id1 | Id2

type Link<'a> = BExpr<LinkTerm<'a>, LinkId>

type Stigmergy<'a> = {
    name:string
    vars:Map<Var,Init> list
    link:Link<'a>
}