[<AutoOpen>]
module Types
open Types
open Link

type Modality =
    | Always
    | Finally

type Quantifier =
    | All
    | Exists

type Property<'a> = {
        name:string
        predicate:BExpr<'a * string option, string>
        modality:Modality
        quantifiers: Map<string, string * Quantifier>
    }

   
[<StructuredFormatDisplay("{AsString}")>]
type 'a BASE =
    | Nil of pos:FParsec.Position
    | Skip of pos:FParsec.Position
    | Act of 'a Types.Action * pos:FParsec.Position
    | Name of string * pos:FParsec.Position
    | Guard of BExpr<'a, unit> * 'a PROC * pos:FParsec.Position
    | Paren of 'a PROC * pos:FParsec.Position
with 
    member this.AsString = this.ToString()
    member this.Pos = 
        match this with
        | Nil(pos=p) | Skip(pos=p) | Act(pos=p)
        | Act(pos=p) | Name(pos=p) | Guard(pos=p) | Paren(pos=p) -> p


and 'a SEQ = 'a BASE list
and 'a CHOICE = 'a SEQ list
and 'a PROC = 'a CHOICE list

let rec walk baseFn seqFn choiceFn parFn p =
     p
     |> List.map (
         List.map (
              List.map (baseFn (walk baseFn seqFn choiceFn parFn))
              >> seqFn) 
          >> choiceFn)
     |> parFn

let rec sprintbase (fn) =
    function
    | Nil _ -> "0"
    | Skip _ -> "√"
    | Paren(p, _) -> fn p
    | Act(a, _) -> string a
    | Guard(b, p, _) -> sprintf "%A -> %s" b (fn p)
    | Name (s, _) -> s

let prettyPrint proc = 
    let parenthesize = sprintf "(%s)"
    walk sprintbase 
        (String.concat "; " >> parenthesize)
        (String.concat " ++ " >> parenthesize)
        (String.concat " || ") proc

type 'a BASE with
    override this.ToString() = sprintbase prettyPrint this




type ComponentDef<'a> = { 
    name: string
    iface: Set<Var>
    lstig: string list
    processes: Map<string, 'a PROC>
}

type SystemDef<'a> = {
    environment: Set<Var>
    stigmergies: Map<string,Stigmergy<'a>>
    components: Map<string, ComponentDef<'a>>
    processes: Map<string, 'a PROC>
    spawn: Map<string, int*int>
    properties: Map<string, Property<'a>>
} with
    member this.ifaceVars = 
        lazy
            this.components
            |> Map.mapValues (fun c -> c.iface)
            |> Map.values
            |> Set.unionMany
            //|> Seq.reduce Map.merge
    member this.lstigVars =
        lazy
            this.stigmergies
            |> Map.mapValues (fun s -> s.vars |> Set.unionMany)
            |> Map.values
            |> Set.unionMany
            //|> Seq.reduce Map.merge

    member this.SpawnedComps = 
        this.components
        |> Map.filter (fun n _ -> this.spawn.ContainsKey n)