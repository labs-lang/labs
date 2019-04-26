namespace Checker
open LabsCore
open Types
open Externs
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
    member this.mapvar (vardef:Node<Var>) = 
        if this.map.ContainsKey vardef.name then this
        else
            let updateMap index = Map.add vardef.name (index, vardef.def.location) this.map
            let updateNext = (+) (match vardef.def.vartype with | Scalar -> 1 | Array n -> n)
            match vardef.def.location with
            | Location.I -> {this with map= updateMap this.nextI; nextI = updateNext this.nextI} 
            | Location.E -> {this with map= updateMap this.nextE; nextE = updateNext this.nextE}            
            | Location.L _ -> {this with map = updateMap this.nextL; nextL = updateNext this.nextL}
        |> zero

/// Type of the symbol table and type-specific data 
type TableType =
    | Agent of parent:SymbolTable
    | Stigmergy of parent: SymbolTable * link: Link<int * Location>
    | Top
    | Top' of spawn: Map<string, int*int>

/// Represents information processed from a Def<>
and SymbolTable = {
    source: Node<unit>
    tableType: TableType
    mapping: Mapping
    variables: Map<string, Var>
    processes : Map<string, Node<Process<int*Location, Position>>>
    children: Map<string, SymbolTable>
}

with
    static member empty source typ =
        {
            source = {pos=source.pos; name=source.name; def=()}
            tableType=typ
            mapping=Mapping.empty
            processes=Map.empty
            variables=Map.empty
            children=Map.empty
        }

module SymbolTable = 

    let rec walk fnode fmerge table =
        let recurse = walk fnode fmerge
        match table.tableType with
        | Top' _
        | Top -> fnode table
        | Agent t
        | Stigmergy(t, _) -> fmerge (fnode table) (recurse t)
    
    let mapVar (v:Node<Var>) table =
        table.mapping.mapvar v
        <~> fun m' -> zero {table with mapping = m'}
        
    let tryAddVar (vardef:Node<Var>) table =
        // TODO SOLVE EXTERNS IN DEF.INIT
        zero {table with variables = Map.add vardef.name vardef.def table.variables}
    
    /// Basic function to retrieve the mapping of variable named k
    let findString table k =
        let find_ t = t.mapping.TryFind k in walk find_ (Option.orElse) table
        //TODO add correct position
        |> function Some x -> x | None -> raise (LabsException ({what=UndefRef k; where=[table.source.pos]})) 
    
    let toVarRef f r o =
        {var=f r.var; offset=o}
    let toVarExpr f e = 
        Expr.map id (toVarRef f) e
    let toVarBExpr f b =
        BExpr.map (BLeaf) (toVarExpr f) b
    
    let toVarProcess table proc =
        let toVarBase f b =    
            match b.stmt with 
            | Act a ->
                let newupdates =
                    a.updates
                    |> List.map (fun (r, e) -> 
                        toVarRef f r (Option.map (toVarExpr f) r.offset), toVarExpr f e)
                {stmt=Act {actionType=a.actionType; updates=newupdates}; pos=b.pos}
            | Nil -> {stmt=Nil; pos=b.pos}
            | Skip -> {stmt=Skip; pos=b.pos}
            | Name s -> {stmt=Name s; pos=b.pos}
        Process.map ((toVarBase (findString table)) >> BaseProcess) (toVarBExpr (findString table)) proc
    
    let tryAddProcess externs (p: Node<Process<_,_>>) table =
        let p' = map (Process.simplify >> toVarProcess table >> Process.replaceExterns externs) p
        zero {table with SymbolTable.processes = Map.add p.name p' table.processes}
    
    let tryAddStigmergy externs (s: Node<Stigmergy<string>>) table =
        let link = map (BExpr.replaceExterns externs >> toVarBExpr (fun (x,y) -> findString table x, y)) s.def.link 
        zero (SymbolTable.empty s (Stigmergy(table, link.def)))
        <~> fun x -> zero {table with children= Map.add s.name x table.children}
    
    let tryAddAgent externs (a:Node<Agent>) table =
        // Keep track of the unfolded process
        let chpos name pos (b:Base<_,Position>) =
            let p=b.pos in let p' = Position((sprintf "%s@%O" name pos), p.Index, p.Line, p.Column)
            {b with pos = p'}
                
        zero (SymbolTable.empty a (Agent(table)))
        <~> fold tryAddVar a.def.iface
        <~> fold (tryAddProcess externs) a.def.processes
        <?> fun x ->
            (* expand the Behavior process *)
            let procmap = walk (fun x -> x.processes |> Map.mapValues (fun x -> x.def)) (Map.union) x
            try 
                let expandedBehavior = {x.processes.["Behavior"] with def = Process.expand chpos procmap "Behavior"}
                zero {x with processes = x.processes.Add("Behavior", expandedBehavior)}
            with err -> wrap table [] [{what=UndefProcess err.Message; where=[a.pos]}] //TODO add correct position
        <~> fun x -> zero {table with children = Map.add a.name x table.children} 