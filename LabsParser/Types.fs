[<AutoOpen>]
module Types
open LabsCore.ExprTypes
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


type Stigmergy<'a> =
    {
        Name: string
        Vars: Set<Node<Var<Expr<unit, unit>>>> list
        Link: Node<Link<'a>>
    }


type QuantPredicate<'a> =
    {
        Predicate:BExpr<'a * string option, string>
        Quantifiers: Map<string, string * Quantifier>
    }
    override this.ToString() =
       let quants =
           this.Quantifiers
           |> Map.map (fun varName (agentType, quant) -> $"{quant} {agentType} {varName}") 
           |> Map.values
           |> String.concat ", "
       let trailingComma = if this.Quantifiers.IsEmpty then "" else ","
       $"{quants}{trailingComma} {this.Predicate}"


type Scope<'a> = 
    | Between of openScope: QuantPredicate<'a> * closeScope: QuantPredicate<'a>
    | FromUntil of openScope: QuantPredicate<'a> * closeScope: QuantPredicate<'a>
    override this.ToString() =
        match this with
        | Between (openScope, closeScope) -> $"between {openScope} and {closeScope},"
        | FromUntil (openScope, closeScope) -> $"from {openScope} until {closeScope},"

type Modality<'a> =
    | Always
    | Finally
    | Eventually
    | Fairly
    | FairlyInf
    | ThereIs of scope: Scope<'a>
    | Globally of scope: Scope<'a>
    | Precedes of scope: Scope<'a> * precedent: QuantPredicate<'a>
    override this.ToString() =
        match this with
        | ThereIs scope -> $"{scope} thereIs"
        | Globally scope -> $"{scope} always"
        | Precedes (scope, prec) -> $"{scope} {prec} precedes"
        | _ -> this.Name
        
    member this.Name =
        match this with
        | ThereIs _ -> "thereIs" | Globally _ -> "always" | Precedes _ -> "precedes"
        | Always -> "always"
        | Finally -> "finally"
        | Eventually -> "eventually"
        | Fairly -> "fairly"
        | FairlyInf -> "fairly_inf"
        

type Property<'a> =
    {
        Name: string
        Modality: Modality<'a>
        QuantPredicate: QuantPredicate<'a> 
    }
    override this.ToString() = $"{this.Modality} {this.QuantPredicate}" 
       

let inline _qpred x =
    let getter prop = prop.QuantPredicate
    let setter prop qp' = {QuantPredicate=qp'; Name=prop.Name; Modality=prop.Modality}
    lens getter setter x

let inline _modality x =
    let getter prop = prop.Modality
    let setter prop m' = {QuantPredicate=prop.QuantPredicate; Name=prop.Name; Modality=m'}
    lens getter setter x


        
let inline _predicate x =
    let getter qp = qp.Predicate
    let setter qp pred' = {Quantifiers=qp.Quantifiers; Predicate=pred'}
    lens getter setter x



type Ast = Node<Sys> * Node<Stigmergy<string>> list * Node<Agent> list * Node<Property<string>> list