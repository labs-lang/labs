module Types

type Location = 
    | I 
    | L of name:string 
    | E
    override this.ToString() =
        match this with L(n) -> n | I -> "I" | E -> "E"

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

type Expr<'a, 'b> =
    | Id of 'b
    | Const of int
    | Ref of Ref<'a, 'b>
    | Abs of Expr<'a, 'b>
    | Arithm of Expr<'a, 'b> * ArithmOp * Expr<'a, 'b>
    override this.ToString() = 
        match this with
        | Id _ -> "id"
        | Const v -> string v
        | Ref r -> string r
        | Abs e -> sprintf "abs(%O)" e
        | Arithm(e1, op, e2) -> sprintf "%O %O %O" e1 op e2

and Ref<'a, 'b> = 
    {var:'a; offset: Expr<'a, 'b> option}
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

type Bop =
    | Conj
    | Disj

///<summmary>Boolean expressions.</summary>
type BExpr<'a, 'b> =
    | True
    | False
    | Compare of Expr<'a, 'b> * CmpOp * Expr<'a, 'b>
    | Neg of BExpr<'a, 'b>
    | Compound of BExpr<'a, 'b> * Bop * BExpr<'a, 'b>

type Action<'a> =
    | AttrUpdate of target:Ref<'a, unit> * expr:Expr<'a, unit>
    | LStigUpdate of target:Ref<'a, unit> * expr:Expr<'a, unit>
    | EnvWrite of target:Ref<'a, unit> * expr:Expr<'a, unit>
    override this.ToString() = 
        match this with
        | AttrUpdate(r, e) -> sprintf "%O <- %O" r e
        | LStigUpdate(r, e) -> sprintf "%O <~ %O" r e
        | EnvWrite(r, e) -> sprintf "%O <-- %O" r e

type Process<'a> = 
    | Nil
    | Skip
    | Base of Action<'a>
    | Seq of Process<'a> * Process<'a>
    | Choice of Process<'a> * Process<'a>
    | Par of Process<'a> * Process<'a>
    | Await of BExpr<'a, unit> * Process<'a>
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

 /// Initialization values
 type Init =
     | Choose of int list
     | Range of int * int
     | Undef