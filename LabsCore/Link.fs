module Link
open Types


type LinkTerm =
| ConstTerm of int
| KeyRefC1 of k:string
| KeyRefC2 of k:string

type LinkExpr =
| T of LinkTerm
| Abs of LinkExpr
| Arithm of LinkExpr * ArithmOp * LinkExpr

type Link = 
| True
| Compare of LinkExpr * CmpOp * LinkExpr
| Neg of Link
| Conj of Link * Link

