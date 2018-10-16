module Link
open Types

type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Stigmergy<'a> = {
    name:string
    vars:Set<Var> list
    link:Link<'a>
}