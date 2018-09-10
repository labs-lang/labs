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


[<StructuredFormatDisplay("{AsString}")>]
type ArithmOp =
    | Plus
    | Minus
    | Times
    | Mod
    member this.AsString = this.ToString()
    override this.ToString() = 
        match this with
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Mod -> "%"
        
[<StructuredFormatDisplay("{AsString}")>]
type Expr<'a> =
    | Const of int
    | Ref of var:'a * offset:Expr<'a> option
    | Arithm of Expr<'a> * ArithmOp * Expr<'a>
    with 
        member this.AsString = this.ToString()
        override this.ToString() = 
            match this with
            | Const v -> v.ToString()
            | Ref(r, offset) -> 
                match offset with
                | Some e -> sprintf "%A[%A]" r e
                | None -> sprintf "%A" r
            | Arithm(e1, op, e2) -> sprintf "%A %A %A" e1 op e2
        
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

[<StructuredFormatDisplay("{AsString}")>]
type Action<'a> =
    | AttrUpdate of target:'a * offset:Expr<'a> option * expr:Expr<'a>
    | LStigUpdate of target:'a * offset:Expr<'a> option * expr:Expr<'a>
    | EnvWrite of target:'a * offset:Expr<'a> option * expr:Expr<'a>
    member this.AsString = this.ToString()
    override this.ToString() = 
        let printTarget(k, o) = 
            match o with
            | Some e -> sprintf "%A[%A]" k e
            | None -> sprintf "%A" k
        match this with
        | AttrUpdate(k, o, e) -> sprintf "%s <- %A" (printTarget(k,o)) e
        | LStigUpdate(k, o, e) -> sprintf "%s <~ %A" (printTarget(k,o)) e
        | EnvWrite(k, o, e) -> sprintf "%s <= %A" (printTarget(k,o)) e

[<StructuredFormatDisplay("{AsString}")>]
type Process<'a> = 
    | Nil
    | Skip
    | Base of Action<'a>
    | Seq of Process<'a> * Process<'a>
    | Choice of Process<'a> * Process<'a>
    | Par of Process<'a> * Process<'a>
    | Await of BExpr<'a> * Process<'a>
    | Name of string
    static member monoid left right op = 
        match left,right with
        | Skip, Skip -> Skip
        | _, Skip -> left
        | Skip, _ -> right
        | _ -> op(left, right)
    static member ( ^. )(left: Process<'a>, right: Process<'a>) =
        Process<'a>.monoid left right Seq
    static member ( ^+ )(left: Process<'a>, right: Process<'a>) =
        Choice(left, right)
    static member ( ^| )(left: Process<'a>, right: Process<'a>) =
        Process<'a>.monoid left right Par
    member this.AsString = this.ToString()        
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Skip -> "√"
        | Base(a) -> a.ToString()
        | Seq(p, q) -> sprintf "%s; %s" p.AsString q.AsString 
        | Choice(p, q) -> sprintf "%s + %s" p.AsString q.AsString
        | Par(p, q) -> sprintf "%s | %s" p.AsString q.AsString
        | Await(b, p) -> sprintf "%A -> %s" b p.AsString
        | Name(s) -> s