[<AutoOpen>]
module Types
open LabsCore.Grammar
open FSharpPlus.Lens

type Sys = {
    Environment: Node<Var<Expr<unit, unit>>> list
    Externals: string list
    Spawn: Node<Expr<unit, unit>> list
    Processes: Node<Process<string>> list
}

type Agent =
    {
        Name: string
        Iface: Node<Var<Expr<unit, unit>>> list
        Lstig: string list
        Processes: Node<Process<string>> list
    }

type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Stigmergy<'a> =
    {
        Name: string
        Vars: Set<Node<Var<Expr<unit, unit>>>> list
        Link: Node<Link<'a>>
    }

type Modality =
    | Always
    | Finally

type Quantifier =
    | All
    | Exists

type Property<'a> =
    {
        Name:string
        Predicate:BExpr<'a * string option, string>
        Modality:Modality
        Quantifiers: Map<string, string * Quantifier>
    }
let inline _predicate x =
    let getter p = p.Predicate
    let setter p pred' = {Predicate=pred'; Name=p.Name; Quantifiers=p.Quantifiers; Modality=p.Modality}
    lens getter setter x
    

type Ast = Node<Sys> * Node<Stigmergy<string>> list * Node<Agent> list * Node<Property<string>> list