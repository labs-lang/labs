[<AutoOpen>]
module Types
open Types
open Link

/// Initialization values
type Init =
    | ChooseI of int list
    | RangeI of int * int
    
type ComponentDef = { 
    name: string
    iface: Map<Key, Init>
    lstig: Map<Key, Init> list
    behavior: string
    processes: Map<string, Process>
}

type SystemDef = {
    environment: Map<Key, Init>
    components: Map<string, ComponentDef>
    processes: Map<string, Process>
    spawn: Map<string, int*int>
    properties: Map<string, TemporalProperty>
    link: Link
}