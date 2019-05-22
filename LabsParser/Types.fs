[<AutoOpen>]
module Types
open LabsCore
open Types
open FSharpPlus.Lens

    
type VarType<'a> = 
    | Scalar
    | Array of size:'a

type Var<'a> = {
        name: string
        vartype: VarType<'a>
        location: Location
        init:Init
    }
    with 
        override this.ToString() = this.name
            
let inline _vartype x =
    let getter v = v.vartype
    let setter v t' = {vartype=t'; name=v.name; location=v.location; init=v.init}
    lens getter setter x

//TODO see if active patterns would be better
let inline isEnvVar v = match v.location with E -> true | _ -> false
let inline isLstigVar v = match v.location with L _ -> true | _ -> false


type Sys = {
    environment: Node<Var<Expr<unit, unit>>> list
    externals: string list
    spawn: Node<Expr<unit, unit>> list
    processes: Node<Process<string>> list
}

type Agent =
    {
        name: string
        iface: Node<Var<Expr<unit, unit>>> list
        lstig: string list
        processes: Node<Process<string>> list
    }

type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Stigmergy<'a> =
    {
        name: string
        vars: Set<Node<Var<Expr<unit, unit>>>> list
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
let inline _predicate x =
    let getter p = p.predicate
    let setter p pred' = {predicate=pred'; name=p.name; quantifiers=p.quantifiers; modality=p.modality}
    lens getter setter x
    

type Ast = Node<Sys> * Node<Stigmergy<string>> list * Node<Agent> list * Node<Property<string>> list