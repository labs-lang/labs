module Checker.Types
open FParsec
open FSharpPlus
open FSharpPlus.Data
open LabsCore
open Types


type Err =
    | Duplicate of string
    | UndefProcess of string
    | UndefBehavior of string
    | UndefAgent of string

type Warn =
    | Unused of string //TODO

type ErrorMessage =
    {
        what: Err
        where: Position list
    }
type WarningMessage =
    {
        what: Warn
        where: Position list
    }

type CheckResult<'a> =
    | Result of 'a * (WarningMessage list) //* (ErrorMessage list)
    | Error of (WarningMessage list) * (ErrorMessage list)
   
let zero x = Result(x, []) 
let inline wrap x warn err =
    if err = [] then Result(x, warn)
    else Error(warn, err)
    
let check f x =
    match x with
    | Result(x', warn) -> 
        match f x' with
        | Result(_, warn') -> Result(x', Operators.plus warn warn')
        | Error(warn', err) -> Error(Operators.plus warn warn', err)
    | Error _ -> x

let transform f x =
    match x with
    | Result(x', warn) ->
        match f x' with
        | Result(y, warn') -> Result(y, Operators.plus warn warn')
        | Error(warn', err) -> Error(Operators.plus warn warn', err) 
    | Error(w, e) -> Error(w, e)

let inline (<?>) x f = check f x
let inline (<??>) x f = x <?> fun _ -> f

let inline (<~>) x f = transform f x 
let inline (<~~>) x f = x <~> fun _ -> f 

let fold fn lst =
    Seq.fold (fun state x -> state <?> (fun _ -> (fn x))) (zero ()) lst
    

let trFold fn lst state =
    Seq.fold (fun s' x -> s' <~> (fn x)) (zero state) lst

type TableType =
    | Agent of parent:SymbolTable
    | Stigmergy of parent: SymbolTable * link: Link<string>
    | Top
and SymbolTable = {
    name: string
    tableType: TableType
    pos: Position
    mapping: Map<string, int * Location>
    variables: Map<string, Var>
    processes : Map<string, ProcDef>
    children: Map<string, SymbolTable>
    nextI: int
    nextL: int
    nextE: int
}

with
    static member empty name pos typ =
        {
            name=name
            pos=pos
            tableType=typ
            mapping=Map.empty
            processes=Map.empty
            variables=Map.empty
            children=Map.empty
            nextI=0
            nextL=0
            nextE=0
            
        }
    interface INode with
        member this.Pos = this.pos
        member this.Name = this.name

let rec walk fnode fmerge table =
    let recurse = walk fnode fmerge
    match table.tableType with
    | Top -> fnode table
    | Agent t
    | Stigmergy(t, _) -> fmerge (fnode table) (recurse t)

let lookupErr item fn map =
    let item' = item :> INode
    if Map.containsKey (item.Name) map then
        let v' = (fn map.[item.Name]) :>  INode
        [{ErrorMessage.what=Duplicate v'.Name; where=[v'.Pos; item.Pos]}]
    else []
    |> wrap () [] 

let mapVar (v:Var) table =
    if table.mapping.ContainsKey v.name then
        let _, loc = table.mapping.[v.name]
        zero table
        <??> if loc = v.location then wrap () [] [] else (lookupErr v id table.variables)
    else
        let nextOffset =
            match v.vartype with
            | Scalar -> 1
            | Array n -> n
        let index, table' =
            match v.location with
            | Location.I -> table.nextI, {table with nextI = table.nextI + nextOffset} 
            | Location.E -> table.nextE, {table with nextE = table.nextE + nextOffset}              
            | Location.L _ -> table.nextL, {table with nextL = table.nextL + nextOffset}
        zero {table' with mapping = Map.add v.name (index, v.location) table.mapping}
    
let tryAddVar (v:Var) table =
    (lookupErr v id table.variables)
    <~~> zero {table with variables = Map.add v.name v table.variables}

let tryAddProcess (procDef: ProcDef) table =
    (lookupErr procDef id table.processes)
    <~~> zero {table with SymbolTable.processes = Map.add procDef.name procDef table.processes}

let tryAddStigmergy (s: Stigmergy<string>) table =
    zero table
    <??> (lookupErr s id table.children)
    <~~>
        let allvars = s.vars |> Set.unionMany
        zero (SymbolTable.empty s.name s.pos (Stigmergy(table, s.link)))
        <~> trFold tryAddVar allvars
        <~> fun x -> zero {table with children= Map.add s.name x table.children}
        <~> trFold mapVar allvars

let tryAddAgent (a:Agent) table =
    zero table
    <?> fun _ -> (lookupErr a id table.children)
    <~~> 
        zero (SymbolTable.empty a.name a.pos (Agent(table)))
        <~> trFold tryAddProcess a.processes
        <~> trFold tryAddVar a.iface
        <~> fun x -> zero {table with children= Map.add a.name x table.children}
        <~> trFold mapVar (a.iface)
