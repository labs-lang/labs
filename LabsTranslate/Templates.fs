module Templates
open Types

//let rec translateExpr enum = function
//    | Const(Int(i)) -> sprintf "%i" i
//    | Const(String(s)) -> "\"" + s + "\""
//    | Const(P(p1,p2)) -> ""
//    | I(k) -> sprintf "comp[tid].I[%i]" (enum.Item k)
//    | L(k) -> sprintf "comp[tid].Lvalue[%i]" (enum.Item k)
//    | Sum(e1, e2) -> sprintf "( (%s) + (%s) )" (translateExpr e1) (translateExpr e2)

//let rec lstigKeys = function
//    | L(x) -> Set.singleton x
//    | Sum(e1, e2) -> Set.union (lstigKeys e1) (lstigKeys e2)
//    | _ -> Set.empty

//let updateKq enum expr = 
    //expr
    //|> lstigKeys
    //|> Seq.map (fun x -> sprintf "setHin(tid, %i);" (enum.Item x))
    //|> String.concat "\n"

//let attr pc k expr enum next = 
//    sprintf """
//void stmt_attr%i (int tid) {
//    __VERIFIER_assume(comp[tid].pc == %i);
//    int val = %s;
//    attr(tid, %i, val, -1);
//    %s

//    comp[tid].pc = %i
//}""" pc pc (translateExpr enum expr) k (updateKq enum expr) next

/// Encodes the skip process.
let skip pc next = 
    sprintf """
void stmt_nop%i (int tid) {
    __VERIFIER_assume(comp[tid].pc == %i);
    comp[tid].pc == %i;
}""" pc pc next

/// Encodes the stop process
let stop pc = 
    sprintf """
void stmt_nop%i (int tid) {
    __VERIFIER_assume(comp[tid].pc == %i);
    __VERIFIER_assume(0);
}""" pc pc