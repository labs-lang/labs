[<AutoOpen>]
module Types
open Types
open Link

/// Initialization values
type Init =
    | Choose of int list
    | Range of int * int

type Property<'a> = 
    | Prop of BExpr<'a>
    | All of comp:string * name:string * Property<'a>
    | Exists of comp:string * name:string * Property<'a>

type TemporalProperty<'a> =
    | Finally of Property<'a>
    | Always of Property<'a>

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
    properties: Map<string, TemporalProperty<'a * string>>
    link: Link<'a>
}