module LabsCore.Grammar

open System
open FParsec
open FSharpPlus.Lens
open LabsCore.ExprTypes
open LabsCore.Expr

type Node<'a> = {
    Name: string
    Pos: Position
    Source: string
    Def: 'a
}
let inline _name x =
    let getter {Name=n} = n
    let setter {Pos=p; Def=d; Source=s} n' = {Name=n'; Pos=p; Def=d; Source=s}
    lens getter setter x
let inline _def x =
    let getter {Def=d} = d
    let setter {Name=n; Pos=p; Source=s } d' = {Name=n; Pos=p; Def=d'; Source=s}
    lens getter setter x
let inline _pos x =
    let getter {Pos=p} = p
    let setter {Name=n; Def=d; Source=s} p' = {Name=n; Pos=p'; Def=d; Source=s}
    lens getter setter x

let inline byName v = (^T : (member Name : string) v)


type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Location =
    | I 
    | L of name:string * tupleIndex: int
    | E
    | Local
    | Pick of num:int * agentType:string option * where: Link<string> option
    override this.ToString() =
        match this with 
            | I -> "Interface" | E -> "Environment"
            | L(n, _) -> $"Stigmergy ({n})" 
            | Local -> "Local"
            | Pick (n, typ, w) ->
                let fmtType = typ |> Option.map (fun x -> $" {x}") |> Option.defaultValue ""
                let fmtWhere = w |> Option.map (fun x -> $" where {x}") |> Option.defaultValue ""
                $"Pick {n}{fmtType}{fmtWhere}"

type Action<'a> = {
    ActionType: Location
    Updates: (Ref<'a, unit> * Expr<'a, unit>) list
    }
    with 
        override this.ToString() =
            (match this.ActionType with
            | Pick _->
                let pick = (string this.ActionType)[1 ..] |> sprintf "p%s"
                fun v _ -> $"{v} := {pick}"
            | Local _ -> sprintf "%s := %s"
            | I -> sprintf "%s <- %s"
            | L _ -> sprintf "%O <~ %O"
            | E -> sprintf "%O <-- %O")
                (this.Updates |> List.map (string << fst) |> String.concat ",")
                (this.Updates  |> List.map (string << snd) |> String.concat ",")


/// Initialization values
type Init =
     | Choose of Expr<unit,unit> list
     | Range of Expr<unit,unit> * Expr<unit,unit>
     | Undef
with
     member this.Pick id_ =
         let rnd = Random()
         let pickRandom lst = List.item (rnd.Next(List.length lst)) lst
             
         match this with
         | Undef -> -128
         | Choose l -> l |> List.map (evalConstExpr (fun _ -> id_)) |> pickRandom
         | Range(minValueExpr, boundExpr) ->
             let minValue = evalConstExpr (fun _ -> id_) minValueExpr
             let bound = evalConstExpr (fun _ -> id_) boundExpr
             rnd.Next(bound-minValue)+minValue
             
     
     override this.ToString() =
        match this with
        | Choose l -> l |> List.map (sprintf "%O") |> String.concat "," |> sprintf "[%s]"
        | Range(min, max) -> $"{min}..{max}"
        | Undef -> "undef"


type Stmt<'a> = 
    | Nil 
    | Skip
    | Act of 'a Action
    | Block of Action<'a> list
    | Name of string
with
    override this.ToString() =
        match this with
        | Nil -> "0"
        | Skip -> "√"
        | Act a -> string a
        | Block stmts ->
            List.map string stmts
            |> String.concat "; "
            |> sprintf "{ %s }"
        | Name s -> s

type Composition =
        | Seq
        | Choice
        | Par
        
type Process<'a> =
    | BaseProcess of Node<Stmt<'a>>
    | Guard of Node<BExpr<'a, unit> * Process<'a>>
    | FatGuard of Node<BExpr<'a, unit> * Process<'a>>
    | Comp of Composition * Process<'a> list

type VarType<'a> = 
    | Scalar
    | Array of size:'a list

type Var<'a> = {
        Name: string
        Vartype: VarType<'a>
        Location: Location
        Init: Init
    }
    with 
        override this.ToString() = this.Name
        
let inline isEnvVar v = match v.Location with E -> true | _ -> false
let inline isLstigVar v = match v.Location with L _ -> true | _ -> false        
            
let inline _vartype x =
    let getter v = v.Vartype
    let setter v t' = {Vartype=t'; Name=v.Name; Location=v.Location; Init=v.Init}
    lens getter setter x