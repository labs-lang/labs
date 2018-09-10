[<AutoOpen>]
module Types
open Types
open Link

/// Initialization values
type Init =
    | Choose of int list
    | Range of int * int

type Modality =
    | Always
    | Finally

type Quantifier =
    | All
    | Exists

type Property<'a> = {
        name:string
        predicate:BExpr<'a * string>
        modality:Modality
        quantifiers: Map<string, string * Quantifier>
    }


type ComponentDef<'a> = { 
    name: string
    iface: Map<Var, Init>
    lstig: Map<Var, Init> list
    behavior: string
    processes: Map<string, Process<'a>>
}



type SystemDef<'a> = {
    environment: Map<Var, Init>
    components: Map<string, ComponentDef<'a>>
    processes: Map<string, Process<'a>>
    spawn: Map<string, int*int>
    properties: Map<string, Property<'a>>
    link: Link<'a>
}