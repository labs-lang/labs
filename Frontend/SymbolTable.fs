namespace Frontend
open FSharpPlus.Lens
open LabsCore
open Grammar
open Externs
open Message
open Outcome
open STS

type Mapping = {
    Map: Map<string, Var<int> * int>
    NextI: int
    NextL: int
    NextE: int
}
with
    static member empty = {Map=Map.empty; NextI=0; NextE=0; NextL=0}
    member this.Item with get(key:string) = this.Map.[key]
    member this.TryFind key = this.Map.TryFind key
    member this.IndexOf (var:Var<_>) = snd this.[var.Name]
    member this.RangeOf (var:Var<_>) =
        this.IndexOf var, this.IndexOf var + (match var.Vartype with Scalar -> 0 | Array i -> i) 
    member this.Mapvar (var:Var<_>) = 
        if this.Map.ContainsKey var.Name then this
        else
            let updateMap index = Map.add var.Name (var, index) this.Map
            let updateNext = (+) (match var.Vartype with | Scalar -> 1 | Array n -> n)
            match var.Location with
            | I -> {this with Map= updateMap this.NextI; NextI = updateNext this.NextI} 
            | E -> {this with Map= updateMap this.NextE; NextE = updateNext this.NextE}            
            | L _ -> {this with Map = updateMap this.NextL; NextL = updateNext this.NextL}

type AgentTable = {
    Sts: TransitionSystem
    Processes: Map<string, Process<Var<int>*int>>
    InitCond: ExitCond
    Variables: Var<int> list
    Lstig: Set<string>
}
with
    static member empty =
        {
            Sts=Set.empty
            Processes=Map.empty
            InitCond=Map.empty
            Variables=[]
            Lstig=Set.empty
        }
    member this.LstigVariables (table:SymbolTable) =
        table.Variables
        |> Map.filter (fun _ v -> match v.Location with | L (s, _) when this.Lstig.Contains s -> true | _ -> false)
        |> Map.values
        |> Seq.sortBy table.M.IndexOf
        
and SymbolTable = {
    Spawn: Map<string, int*int>
    Agents: Map<string, AgentTable>
    Stigmergies: Map<string, Link<Var<int>*int>>
    Processes : Map<string, Process<Var<int>*int>>
    Variables: Map<string, Var<int>>
    M: Mapping
    Guards: Map<Node<Stmt<Var<int>*int>>, Set<BExpr<Var<int>*int, unit>>>
    Properties: Map<string, Node<Property<Var<int>*int>>>
}
with
    static member empty =
        {
            Spawn = Map.empty
            Agents = Map.empty
            Stigmergies = Map.empty
            Processes = Map.empty
            Variables = Map.empty
            M = Mapping.empty
            Guards = Map.empty
            Properties = Map.empty
        }
        
module internal SymbolTable = 
    let mapVar (v:Var<_>) table =
        zero {table with M = table.M.Mapvar v}
    
    let private processVar externs vardef =
        let toVarInt var =
            {
                Location=var.Location;
                Name=var.Name;
                Vartype=match var.Vartype with Scalar -> Scalar | Array e -> Array (Expr.evalCexprNoId e);
                Init=var.Init
            }
        
        map (Var.replaceExterns externs) vardef
        |> map toVarInt
        |> fun x ->
            match x.Def.Vartype with
            | Array s when s <= 0 -> 
                raise (LabsException {What=NonPositiveArraySize vardef.Name; Where=[vardef.Pos]})
            | _ -> x
            
    let tryAddVar externs (vardef:Node<Var<Expr<unit, unit>>>) table =
        if table.Variables.ContainsKey vardef.Name then
            raise (LabsException {What=Generic (sprintf "Unexpected operation on variable %s" vardef.Name); Where=[vardef.Pos]})
        else zero {table with Variables = table.Variables.Add(vardef.Name, (processVar externs vardef).Def)}
    
    /// Basic function to retrieve the mapping of variable named k
    let findString table k =
        //TODO add correct position
        table.M.TryFind k
        |> function Some x -> x | None -> raise (LabsException ({What=UndefRef k; Where=[]})) 
    
    let toVarRef f r o =
        {Var=f r.Var; Offset=o}
    let toVarExpr f e = 
        Expr.map id (toVarRef f) e
    let toVarBExpr f b =
        BExpr.map (BLeaf) (toVarExpr f) b
    
    let toVarProcess table proc =
        let toVarBase f b =    
            match b.Def with 
            | Act a ->
                let newupdates =
                    a.Updates
                    |> List.map (fun (r, e) -> 
                        toVarRef f r (Option.map (toVarExpr f) r.Offset), toVarExpr f e)
                Act {ActionType=a.ActionType; Updates=newupdates}
            | Nil -> Nil
            | Skip -> Skip
            | Name s -> Name s
            |> fun def' -> {Def=def'; Pos=b.Pos; Name=b.Name; Source=b.Source}
        Process.map ((toVarBase (findString table)) >> BaseProcess) (toVarBExpr (findString table)) proc
    
    let private handleProcessNode externs table p =
        map (Process.simplify >> Process.fixGuardedPar >> toVarProcess table >> Process.replaceExterns externs) p
    
    /// <summary>Builds a map from actions to guards.</summary>
    /// <remarks>This function assumes that all occurrences of the form
    /// (guard -> Par(...))
    /// have been transformed into
    /// (guard -> Seq([Skip; Par(...)). 
    /// </remarks>
    let setGuards proc =
        let baseFn (guards, acc) b = (guards, Map.add b guards acc)
        let guardFn (guards, acc) g = (Set.add g guards, acc)
        let compFn typ recurse (guards, acc) (l:List<_>) =
            if l.IsEmpty then (guards, acc) else
            match typ with
            | Seq ->
                // Guards only affect the first process in a sequence
                let (_, acc') = recurse (guards, acc) l.Head
                recurse (Set.empty, acc') (Comp(Seq, l.Tail)) 
            | _ ->
                Seq.map (recurse (guards, acc)) l
                |> Seq.reduce(fun (_, a1) (_, a2) -> Set.empty, Map.union a2 a1)
        Process.fold baseFn guardFn compFn (Set.empty, Map.empty) proc                    
        |> snd
    
    let tryAddProcess externs (p: Node<Process<_>>) table =
        let p' = handleProcessNode externs table p
        zero {table with Processes=Map.add p.Name p'.Def table.Processes; Guards=Map.union table.Guards (setGuards p'.Def)}
    
    let tryAddStigmergy externs (s: Node<Stigmergy<string>>) table =
        let link = map (BExpr.replaceExterns externs >> toVarBExpr (fun (x,y) -> findString table x, y)) s.Def.Link
        zero {table with Stigmergies=table.Stigmergies.Add(s.Name, link.Def)}
    
    let tryAddIface externs (a:Node<Agent>) table =
        let iface = List.map (processVar externs >> (fun x -> x.Def)) a.Def.Iface
        fold mapVar iface table
        <~> fun t -> zero {t with Agents = table.Agents.Add(a.Name, {AgentTable.empty with Variables=iface |> List.sortBy t.M.IndexOf})}
    
    let tryAddAgent externs (a:Node<Agent>) (table, state) =
        List.map ((handleProcessNode externs table) >> (fun node -> node.Name, node.Def)) a.Def.Processes
        |> (Map.ofList >> zero)
        <~> fun p ->
            let allProcesses = Map.union p table.Processes
            let p' = (Map.add "Behavior" (Process.expand allProcesses "Behavior") allProcesses)
            let (lts, acc), initCond = STS.makeTransitions state p'.["Behavior"]
            let lts' = STS.removeNames p' lts
            let guards = Map.union table.Guards (setGuards p'.["Behavior"])
            let agent = {table.Agents.[a.Name] with Processes=p'; Sts=lts'; InitCond=initCond; Lstig=a.Def.Lstig |> Set.ofList}
            zero ({table with Agents = table.Agents.Add(a.Name, agent); Guards=guards}, (Set.empty, acc))        
    
    let makeSpawnRanges externs spawn table =
        let makeRanges mp =
            mp 
            |> Map.fold (fun (c, m) name d -> let c' = c + d.Def in (c', (Map.add name (c, c') m) )) (0, Map.empty) 
            |> snd
        
        let spawn' =
            spawn
            |> List.map (fun (d:Node<_>) -> d.Name, d)
            |> Map.ofList
            |> Map.mapValues (map (Expr.replaceExterns externs >> Expr.evalCexprNoId ))
                     
        let valid, others = Map.partition (fun _ d -> d.Def > 0) spawn'
        let zeroes, negatives = Map.partition (fun _ d -> d.Def = 0) others 
        let warnings =
            zeroes |> Map.mapValues (fun d -> {What=SpawnZero d.Name; Where=[d.Pos]}) |> Map.values
        let errors =
            negatives |> Map.mapValues (fun d -> {What=NegativeSpawn d.Name; Where=[d.Pos]}) |> Map.values

        wrap {table with SymbolTable.Spawn=makeRanges valid} (List.ofSeq warnings) (List.ofSeq errors)

    let tryAddProperty externs (p:Node<Property<string>>) (table:SymbolTable) =
        let fn = (BExpr.replaceExterns externs) >> toVarBExpr (fun (x, y) -> findString table x, y)
        zero {table with Properties= Map.add p.Name (map (over _predicate fn) p) table.Properties}

    let lstigVariablesOf table name =
        table.Variables
        |> Map.filter (fun _ v -> match v.Location with | L (s, _) when table.Agents.[name].Lstig.Contains s -> true | _ -> false)
        |> Map.values
        |> Seq.sortBy table.M.IndexOf
    
    let dump (table:SymbolTable) =
        let dumpVar v =
            match v.Vartype with
            | Scalar -> sprintf "%i=%s=%O" (table.M.IndexOf v) v.Name v.Init
            | Array s -> sprintf "%i=%s[%i]=%O" (snd table.M.[v.Name]) v.Name s v.Init
        let dumpSpawn agentName (_start, _end) =
            let iface = table.Agents.[agentName].Variables |> List.map dumpVar |> String.concat ";"
            let lstig = table.Agents.[agentName].LstigVariables table |> Seq.map dumpVar |> String.concat ";"
            printfn "%s %i,%i\n%s\n%s" agentName _start _end iface lstig
        printfn "%s" (table.Variables |> Map.filter (fun _ v -> isEnvVar v) |> Map.values |> Seq.sortBy table.M.IndexOf |> Seq.map dumpVar |> String.concat ";")
        Map.map (dumpSpawn) table.Spawn |> ignore
        table.Properties
        |> Map.mapValues (fun p -> match p.Def.Modality with Always -> "always" | Finally -> "finally")
        |> Map.values
        |> String.concat ";"
        |> printfn "%s"
        
type SymbolTable with member this.Dump() = SymbolTable.dump this