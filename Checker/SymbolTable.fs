namespace Checker
open LabsCore
open Types
open Externs
open LabsCore
open Message
open Outcome

type Mapping = {
    map: Map<string, int * Location>
    nextI: int
    nextL: int
    nextE: int
}
with
    static member empty = {map=Map.empty; nextI=0; nextE=0; nextL=0}
    member this.Item with get(key:string) = this.map.[key]
    member this.TryFind key = this.map.TryFind key
    member this.mapvar (var:Var) = 
        if this.map.ContainsKey var.name then this
        else
            let updateMap index = Map.add var.name (index, var.location) this.map
            let updateNext = (+) (match var.vartype with | Scalar -> 1 | Array e -> Expr.evalConstExpr e)
            match var.location with
            | Location.I -> {this with map= updateMap this.nextI; nextI = updateNext this.nextI} 
            | Location.E -> {this with map= updateMap this.nextE; nextE = updateNext this.nextE}            
            | Location.L _ -> {this with map = updateMap this.nextL; nextL = updateNext this.nextL}
        
type SymbolTable = {
    spawn: Map<string, int*int>
    agents: Map<string, (Var list) * Map<string, Process<int*Location>>>
    stigmergies: Map<string, Link<int * Location>>
    processes : Map<string, Process<int*Location>>
    variables: Map<string, Var>
    m: Mapping
}
with
    static member empty =
        {
            spawn = Map.empty
            agents = Map.empty
            stigmergies = Map.empty
            processes = Map.empty
            variables = Map.empty
            m = Mapping.empty
        }

module SymbolTable = 
    open Externs

    let mapVar (v:Var) table =
        let m' = table.m.mapvar v
        zero {table with m = m'}
        
    let tryAddVar externs (vardef:Node<Var>) table =
        let vardef' = map (Var.replaceExterns externs) vardef
        if table.variables.ContainsKey vardef.name then
            raise (LabsException {what=Generic (sprintf "Unexpected operation on variable %s" vardef.name); where=[vardef.pos]})
        else zero {table with variables = Map.add vardef.name vardef'.def table.variables}
    
    /// Basic function to retrieve the mapping of variable named k
    let findString table k =
        //TODO add correct position
        table.m.TryFind k
        |> function Some x -> x | None -> raise (LabsException ({what=UndefRef k; where=[]})) 
    
    let toVarRef f r o =
        {var=f r.var; offset=o}
    let toVarExpr f e = 
        Expr.map id (toVarRef f) e
    let toVarBExpr f b =
        BExpr.map (BLeaf) (toVarExpr f) b
    
    let toVarProcess table proc =
        let toVarBase f b =    
            match b.def with 
            | Act a ->
                let newupdates =
                    a.updates
                    |> List.map (fun (r, e) -> 
                        toVarRef f r (Option.map (toVarExpr f) r.offset), toVarExpr f e)
                Act {actionType=a.actionType; updates=newupdates}
            | Nil -> Nil
            | Skip -> Skip
            | Name s -> Name s
            |> fun def' -> {def= def'; pos=b.pos; name=b.name}
        Process.map ((toVarBase (findString table)) >> BaseProcess) (toVarBExpr (findString table)) proc
    
    let tryAddProcess externs (p: Node<Process<_>>) table =
        let p' = map (Process.simplify >> toVarProcess table >> Process.replaceExterns externs) p
        zero {table with processes = Map.add p.name p'.def table.processes}
    
    let tryAddStigmergy externs (s: Node<Stigmergy<string>>) table =
        let link = map (BExpr.replaceExterns externs >> toVarBExpr (fun (x,y) -> findString table x, y)) s.def.link
        zero {table with stigmergies=table.stigmergies.Add(s.name, link.def)}
    
    let tryAddIface externs (a:Node<Agent>) table =
        let iface = List.map (map (Var.replaceExterns externs)) a.def.iface |> List.map (fun x -> x.def)
        fold mapVar iface table
        <~> fun t -> zero {t with agents = table.agents.Add(a.name, (iface, Map.empty))}
    
    let tryAddAgent externs (a:Node<Agent>) table =
        // Keep track of the unfolded process
        let iface = fst table.agents.[a.name]
                map (Process.simplify >> toVarProcess table >> Process.replaceExterns externs)
        |> fun f -> List.map f a.def.processes
        |> List.map (fun node -> node.name, node.def)
        |> Map.ofList |> zero
        <~> fun p ->
            try
                zero (Map.add "Behavior" (Process.expand (Map.union p table.processes) "Behavior") p)
            with err -> wrap (p) [] [{what=UndefProcess err.Message; where=[a.pos]}] //TODO add correct position
        <~> fun p -> zero {table with agents = table.agents.Add(a.name, (iface, p)) }