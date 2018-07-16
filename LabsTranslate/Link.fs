module Link
open Templates
open Link
open Expressions

let rec translateLinkExpr mapping expr = 
    let tlinkexp = translateLinkExpr mapping
    match expr with
    | T(ConstTerm (i)) -> sprintf "%i" i
    | T(KeyRefC1(k)) -> (translateKey mapping "__LABS_link1" k)
    | T(KeyRefC2(k)) -> (translateKey mapping "__LABS_link2" k)
    | Abs(e) -> sprintf "abs(%s)" (tlinkexp e)
    | Arithm(e1, op, e2) ->
        (translateAOp op) (tlinkexp e1) (tlinkexp e2)

let rec translateLink mapping expr = 
    let tlexpr = translateLinkExpr mapping
    let tl = translateLink mapping
    match expr with
    | True -> "1"
    | Compare(a, op, b) -> sprintf "(%s)\n%s\n(%s)" (tlexpr a) (translateBOp op) (tlexpr b) //TODO
    | Neg(a) -> sprintf "!(%s)" (tl a)
    | Conj(a, b) -> sprintf "(%s) && (%s)" (tl a) (tl b)

let encodeLink mapping link =
    translateLink mapping link
    |> sprintf "int __LABS_link = %s;\nreturn __LABS_link;"
    |> (indent 4)
    |> cfunc "int" "link" "int __LABS_link1, int __LABS_link2"