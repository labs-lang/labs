module Types
open Tokens
open FParsec
open FSharpPlus.Lens

type Node<'a> = {
    name: string
    pos: Position
    def: 'a
}
let inline _name x =
    let getter {name=n; pos=p; def=d} = n
    let setter {name=n; pos=p; def=d} n' = {name=n'; pos=p; def=d}
    lens getter setter x
let inline _def x =
    let getter {name=n; pos=p; def=d} = d
    let setter {name=n; pos=p; def=d} d' = {name=n; pos=p; def=d'}
    lens getter setter x
let inline _pos x =
    let getter {name=n; pos=p; def=d} = p
    let setter {name=n; pos=p; def=d} p' = {name=n; pos=p'; def=d}
    lens getter setter x

type Location =
    | I 
    | L of name:string * tupleIndex: int
    | E
    override this.ToString() =
        match this with 
            | I -> "Interface" | E -> "Environment"
            | L(n, _) -> sprintf "Stigmergy (%s)" n 

type ArithmOp =
    | Plus | Minus
    | Times | Div | Mod
    | Min | Max
    override this.ToString() = 
        match this with
        | Plus -> tPLUS | Minus -> tMINUS
        | Times -> tMUL | Div -> tDIV | Mod -> tMOD
        | Min -> tMIN | Max -> tMAX

type UnaryOp = 
    | Abs | UnaryMinus
    override this.ToString() =
        match this with
        | Abs -> tABS
        | UnaryMinus -> tMINUS

type LeafExpr<'b> =
    | Id of 'b
    | Const of int
    | Extern of string
    override this.ToString() = 
        match this with
        | Id _ -> tID
        | Const v -> string v
        | Extern s -> "_" + s
and Expr<'a, 'b> =
    | Leaf of LeafExpr<'b>
    | Ref of Ref<'a, 'b>
    | Unary of UnaryOp * Expr<'a, 'b>
    | Arithm of Expr<'a, 'b> * ArithmOp * Expr<'a, 'b>
    override this.ToString() = 
        match this with
        | Leaf l -> string l
        | Ref r -> string r
        | Unary(op, e) -> 
            let s = match op with Abs -> tABS | UnaryMinus -> tMINUS in sprintf "%s(%O)" s e
        | Arithm(e1, op, e2) ->
            match op with
            | Min | Max -> sprintf "%O(%O, %O)" op e1 e2 
            | _ -> sprintf "%O %O %O" e1 op e2


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
        match this with Conj -> tCONJ | Disj -> tDISJ

///<summmary>Boolean expressions.</summary>
type BExpr<'a, 'b> =
    | BLeaf of bool
    | Compare of Expr<'a, 'b> * CmpOp * Expr<'a, 'b>
    | Neg of BExpr<'a, 'b>
    | Compound of BExpr<'a, 'b> * Bop * BExpr<'a, 'b>
    override this.ToString() =
        match this with
        | BLeaf true -> tTRUE | BLeaf false -> tFALSE
        | Neg b -> sprintf "%s(%O)" tNEG b
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

module Action =
    let updates_ =
        (fun a -> a.updates),
        (fun u a -> {actionType=a.actionType; updates=u})

 /// Initialization values
 type Init =
     | Choose of Expr<unit,unit> list
     | Range of Expr<unit,unit> * Expr<unit,unit>
     | Undef
     override this.ToString() =
        match this with
        | Choose l -> l |> List.map (sprintf "%O") |> String.concat "," |> sprintf "[%s]"
        | Range(min, max) -> sprintf "%O..%O" min max
        | Undef -> "undef"