module Types

open System


let rng = Random()

type Point = int * int

type Val =
    | Int of int
    | P of Point
    static member (+) (left: Val, right: Val) =
        match (left, right) with
        | (Int(a), Int(b)) -> Some(Int(a+b)) // Sum
        | (P(p1), P(p2)) -> Some(P(fst p1 + fst p2, snd p1 + snd p2))
        | _ -> None

    static member (-) (left: Val, right: Val) =
        match (left, right) with
        | (Int(a), Int(b)) -> Some(Int(a-b))
        | (P(p1), P(p2)) -> Some(P(fst p1 - fst p2, snd p1 - snd p2))
        | _ -> None

type Tval = Val * int
type Key = string
type Tpair = Key * Tval
type Interface = Map<Key, Val>

[<StructuredFormatDisplay("{AsString}")>]
type ArithmOp =
| Plus
| Minus
| Times
| Mod
    with 
        member this.AsString = this.ToString()
        override this.ToString() = 
            match this with
            | Plus -> "+" | Minus -> "-" | Times -> "*" | Mod -> "%"

[<StructuredFormatDisplay("{AsString}")>]
type Expr =
    | Const of Val
    | K of Key
    | Arithm of Expr * ArithmOp * Expr
    with 
        member this.AsString = this.ToString()
        override this.ToString() = 
            match this with
            | Const(v) -> v.ToString()
            | K(k) -> k
            | Arithm(e1, op, e2) -> sprintf "%A %A %A" e1 op e2



type Op = 
    | Equal
    | Less
    | Greater

///<summmary>Boolean expressions.</summary>
type BExpr =
    | True
    | False
    | Compare of Expr * Op * Expr
    | Neg of BExpr
    | Conj of BExpr * BExpr

type Action =
| AttrUpdate of Key * Expr
| LStigUpdate of Key * Expr
| EnvWrite of Key * Expr
| EnvRead of Key * Key
with
    override this.ToString() = 
        match this with
        | AttrUpdate(a, e) -> sprintf "I[%s] := %s" a (e.ToString())
        | LStigUpdate(k, v) -> sprintf "L[%s] := %A" k v
        | EnvWrite(k, v) -> sprintf "E[%s] := %A" k v
        | EnvRead(j, k) -> sprintf "I[%s] := E[%s]" j k
        //| Await(b) -> sprintf "%A?" b


[<StructuredFormatDisplay("{AsString}")>]
type Process = 
| Nil
| Skip
| Base of Action
| Seq of Process * Process
| Choice of Process * Process
| Par of Process * Process
| Await of BExpr * Process
| Name of string
with
    static member monoid left right op = 
        match left,right with
        | Skip, Skip -> Skip
        | _, Skip -> left
        | Skip, _ -> right
        | _ -> op(left, right)


    static member ( ^. )(left: Process, right: Process) =
        Process.monoid left right Seq

    static member ( ^+ )(left: Process, right: Process) =
        Process.monoid left right Choice

    static member ( ^| )(left: Process, right: Process) =
        Process.monoid left right Par


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

type MaybeBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x

let maybe = new MaybeBuilder()