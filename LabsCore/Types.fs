module Types

type Location = 
    | I 
    | L of name:string 
    | E
    override this.ToString() =
        match this with 
            | I -> "Interface" | E -> "Environment"
            | L n -> sprintf "Stigmergy (%s)" n 

type VarType = 
    | Scalar
    | Array of size:int
 /// Initialization values
 type Init =
     | Choose of int list
     | Range of int * int
     | Id
     | Undef
     override this.ToString() =
        match this with
        | Choose l -> l |> List.map string |> String.concat "," |> sprintf "[%s]"
        | Range(min, max) -> sprintf "%i..%i" min max
        | Undef -> "undef"
        | Id -> "id"


type Var = {
    name:string
    vartype:VarType
    location:Location
    init:Init
}
with override this.ToString() = this.name

type ArithmOp =
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    override this.ToString() = 
        match this with
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" | Mod -> "%" 

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
    override this.ToString() = 
        match this with
        | Less -> "<"
        | Equal -> "=="
        | Greater -> ">"
        | Leq -> "<="
        | Geq -> ">="
        | Neq -> "!="

type Bop =
    | Conj
    | Disj
    override this.ToString() = 
        match this with Conj -> "and" | Disj -> "or"

///<summmary>Boolean expressions.</summary>
type BExpr<'a, 'b> =
    | True
    | False
    | Compare of Expr<'a, 'b> * CmpOp * Expr<'a, 'b>
    | Neg of BExpr<'a, 'b>
    | Compound of BExpr<'a, 'b> * Bop * BExpr<'a, 'b>
    override this.ToString() =
        match this with
        | True -> "true" | False -> "false"
        | Neg b -> sprintf "!(%O)" b
        | Compare(e1, op, e2) -> sprintf "(%O) %O (%O)" e1 op e2
        | Compound(b1, op, b2) -> sprintf "(%O) %O (%O)" b1 op b2

type Action<'a> = {
    actionType: Location
    updates: (Ref<'a, unit> * Expr<'a, unit>) list
    }
    with 
        override this.ToString() = 
            (match this.actionType with
            | I -> sprintf "%s <- %s"
            | L _ -> sprintf "%O <~ %O"
            | E -> sprintf "%O <-- %O")
                (this.updates |> List.map (string << fst) |> String.concat ",")
                (this.updates  |> List.map (string << snd) |> String.concat ",")

type Process<'a, 'b> = 
    | Nil
    | Skip of 'b
    | Base of Action<'a> * 'b
    | Seq of Process<'a, 'b> * Process<'a, 'b>
    | Choice of Process<'a, 'b> * Process<'a, 'b>
    | Par of Process<'a, 'b> * Process<'a, 'b>
    | Await of BExpr<'a, unit> * Process<'a, 'b>
    | Name of string * 'b
    static member private monoid left right op = 
        match left,right with
        | _, Skip _ -> left
        | Skip _, _ -> right
        | _ -> op(left, right)
    static member ( ^. )(left: Process<'a, 'b>, right: Process<'a, 'b>) =
        Seq(left, right)
    static member ( ^+ )(left: Process<'a, 'b>, right: Process<'a, 'b>) =
        Choice(left, right)
    static member ( ^| )(left: Process<'a, 'b>, right: Process<'a, 'b>) =
        Process<'a, 'b>.monoid left right Par
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Skip _ -> "√"
        | Base (a, _) -> string a
        | Seq(p, q) -> sprintf "%O; %O" p q
        | Choice(p, q) -> sprintf "%O + %O" p q
        | Par(p, q) -> sprintf "%O | %O" p q
        | Await(b, p) -> sprintf "%A -> %O" b p
        | Name (s, _) -> s
