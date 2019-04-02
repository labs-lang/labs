[<AutoOpen>]
module Types
open FParsec
open FParsec
open LabsCore
open Types


//    
//type ComponentDef<'a> = { 
//    name: string
//    iface: Set<Var>
//    lstig: string list
//    processes: Map<string, Process<'a, FParsec.Position>>
//}
//
//type SystemDef<'a> = {
//    environment: Set<Var>
//    stigmergies: Map<string,Stigmergy<'a>>
//    components: Map<string, ComponentDef<'a>>
//    processes: Map<string, Process<'a, FParsec.Position>>
//    spawn: Map<string, int*int>
//    properties: Map<string, Property<'a>>
//} with
//    member this.ifaceVars = 
//        lazy
//            this.components
//            |> Map.mapValues (fun c -> c.iface)
//            |> Map.values
//            |> Set.unionMany
//    member this.lstigVars =
//        lazy
//            this.stigmergies
//            |> Map.mapValues (fun s -> s.vars |> Set.unionMany)
//            |> Map.values
//            |> Set.unionMany  


type INode =
    abstract member Pos: Position
    abstract member Name: string

    
type VarType = 
    | Scalar
    | Array of size:int
type Var = {
        name: string
        vartype: VarType
        location: Location
        position: Position
        init:Init
    }
    with 
        override this.ToString() = this.name
        interface INode with
            member this.Pos = this.position
            member this.Name = this.name

type ProcDef<'a> =
    {
        name: string
        pos: Position
        proc: Process<'a, Position>
    }
    interface INode with
    member this.Pos = this.pos
    member this.Name = this.name

type Sys<'a> = {
    environment: Var list
    externals: string list
    spawn: (Position * string * Expr<unit, unit>) list
    processes: ProcDef<'a> list
}

type Agent<'a> =
    {
        pos: Position
        name: string
        iface: Var list
        lstig: string list
        processes: ProcDef<'a> list
    }
    interface INode with
        member this.Pos = this.pos
        member this.Name = this.name

type LinkComponent = | C1 | C2

type Link<'a> = BExpr<'a * LinkComponent, LinkComponent>

type Stigmergy<'a> =
    {
        pos: Position
        name: string
        vars: Set<Var> list
        link: Link<'a>
    }
    interface INode  with
        member this.Pos = this.pos
        member this.Name = this.name

type Modality =
    | Always
    | Finally

type Quantifier =
    | All
    | Exists

type Property<'a> =
    {
        pos: Position
        name:string
        predicate:BExpr<'a * string option, string>
        modality:Modality
        quantifiers: Map<string, string * Quantifier>
    }
    interface INode  with
        member this.Pos = this.pos
        member this.Name = this.name

type Ast = Sys<string> * Stigmergy<string> list * Agent<string> list * Property<string> list