[<AutoOpen>]
module Types
open FParsec
open LabsCore
open Types

type Node<'a> = {
    name: string
    pos: Position
    def: 'a
}
    
type VarType = 
    | Scalar
    | Array of size:int

type Var = {
        name: string
        vartype: VarType
        location: Location
        init:Init
    }
    with 
        override this.ToString() = this.name

type Sys = {
    environment: Node<Var> list
    externals: string list
    spawn: Node<Expr<unit, unit>> list
    processes: Node<Process<string, Position>> list
}

type Agent =
    {
        name: string
        iface: Node<Var> list
        lstig: string list
        processes: Node<Process<string, Position>> list
    }

type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Stigmergy<'a> =
    {
        name: string
        vars: Set<Node<Var>> list
        link: Node<Link<'a>>
    }

type Modality =
    | Always
    | Finally

type Quantifier =
    | All
    | Exists

type Property<'a> =
    {
        name:string
        predicate:BExpr<'a * string option, string>
        modality:Modality
        quantifiers: Map<string, string * Quantifier>
    }

type Ast = Node<Sys> * Node<Stigmergy<string>> list * Node<Agent> list * Node<Property<string>> list