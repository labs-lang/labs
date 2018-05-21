[<AutoOpen>]
module Types
open Types
open Link

/// Initialization values
type Init =
    | ChooseI of int list
    | ChooseP of Point list
    | RangeI of int * int
    | RangeP of Point * Point

type ComponentDef = {
    name: string
    iface: Map<Key, Init>; 
    lstig: Map<Key, Init>; 
    behavior: string;
    processes: Map<string, Process>    
}
with
    member this.allKeys = 
        [this.iface; this.lstig] |> Seq.map Map.keys |> Set.unionMany

type SystemDef = {
    environment: Map<Key, Init>;
    components: Map<string, ComponentDef>;
    processes: Map<string, Process>;
    spawn: Map<string, int*int>;
    properties: Map<string, TemporalProperty>
    link: Link
}