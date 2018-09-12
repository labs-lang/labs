module Types

type Location = | I | L | E
type VarType = 
    | Scalar
    | Array of size:int
type Var = {
    name:string
    vartype:VarType
    location:Location
}
with override this.ToString() = this.name


type ArithmOp =
    | Plus
    | Minus
    | Times
    | Mod
    override this.ToString() = 
        match this with
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Mod -> "%"

type Expr<'a> =
    | Const of int
    | Ref of Ref<'a>
    | Arithm of Expr<'a> * ArithmOp * Expr<'a>
    override this.ToString() = 
        match this with
        | Const v -> string v
        | Ref(r) -> string r
        | Arithm(e1, op, e2) -> sprintf "%O %O %O" e1 op e2

and Ref<'a> = 
    {var:'a; offset: Expr<'a> option}
    override this.ToString() = 
        match this.offset with
        | Some e -> sprintf "%O[%O]" this.var e
        | None -> this.var.ToString()

type CmpOp = 
    | Equal
    | Greater
    | Less
    | Leq
    | Geq
    | Neq

///<summmary>Boolean expressions.</summary>
type BExpr<'a> =
    | True
    | False
    | Compare of Expr<'a> * CmpOp * Expr<'a>
    | Neg of BExpr<'a>
    | Conj of BExpr<'a> * BExpr<'a>

type Action<'a> =
    | AttrUpdate of target:Ref<'a> * expr:Expr<'a>
    | LStigUpdate of target:Ref<'a> * expr:Expr<'a>
    | EnvWrite of target:Ref<'a> * expr:Expr<'a>
    override this.ToString() = 
        match this with
        | AttrUpdate(r, e) -> sprintf "%O <- %O" r e
        | LStigUpdate(r, e) -> sprintf "%O <~ %O" r e
        | EnvWrite(r, e) -> sprintf "%O <= %O" r e

type Process<'a> = 
    | Nil
    | Skip
    | Base of Action<'a>
    | Seq of Process<'a> * Process<'a>
    | Choice of Process<'a> * Process<'a>
    | Par of Process<'a> * Process<'a>
    | Await of BExpr<'a> * Process<'a>
    | Name of string
    static member private monoid left right op = 
        match left,right with
        | _, Skip -> left
        | Skip, _ -> right
        | _ -> op(left, right)
    static member ( ^. )(left: Process<'a>, right: Process<'a>) =
        Process<'a>.monoid left right Seq
    static member ( ^+ )(left: Process<'a>, right: Process<'a>) =
        Choice(left, right)
    static member ( ^| )(left: Process<'a>, right: Process<'a>) =
        Process<'a>.monoid left right Par
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Skip -> "√"
        | Base a -> string a
        | Seq(p, q) -> sprintf "%O; %O" p q
        | Choice(p, q) -> sprintf "%O + %O" p q
        | Par(p, q) -> sprintf "%O | %O" p q
        | Await(b, p) -> sprintf "%A -> %O" b p
        | Name s -> s
