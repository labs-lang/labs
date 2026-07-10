namespace Frontend

open System.Text.Json.Serialization
open System.Text.RegularExpressions
open FSharpPlus.Operators
open FSharpPlus.Lens
open Frontend
open LabsCore
open LabsCore.Tokens
open LabsCore.Grammar
open LabsCore.ExprTypes
open LabsCore.Expr
open Externs
open Message
open Outcome
open STS

type Mapping =
    { Map: Map<string, Var<int> * int>
      NextI: int
      NextL: int
      NextE: int }

    static member empty =
        { Map = Map.empty
          NextI = 0
          NextE = 0
          NextL = 0 }

    member this.Item
        with get (key: string) = this.Map[key]

    member this.TryFind key = this.Map.TryFind key
    member this.IndexOf(var: Var<_>) = snd this[var.Name]

    member this.RangeOf(var: Var<_>) =
        this.IndexOf var,
        this.IndexOf var
        + (match var.Vartype with
           | Array s -> List.reduce (*) s
           | _ -> 0)

    member this.Mapvar(var: Var<_>) =
        if this.Map.ContainsKey var.Name then
            this
        else
            let updateMap index = Map.add var.Name (var, index) this.Map

            let updateNext =
                (+) (
                    match var.Vartype with
                    | Array n -> List.reduce (*) n
                    | _ -> 1
                )

            match var.Location with
            | I ->
                { this with
                    Map = updateMap this.NextI
                    NextI = updateNext this.NextI }
            | E ->
                { this with
                    Map = updateMap this.NextE
                    NextE = updateNext this.NextE }
            | L _ ->
                { this with
                    Map = updateMap this.NextL
                    NextL = updateNext this.NextL }
            | Local
            | Pick _ -> this

type AgentTable =
    { Sts: TransitionSystem
      Behavior: string
      Processes: Map<string, Process<Var<int> * int>>
      InitCond: ExitCond
      Variables: Var<int> list
      Lstig: Set<string> }

    static member empty =
        { Sts = Set.empty
          Behavior = "Behavior"
          Processes = Map.empty
          InitCond = Map.empty
          Variables = []
          Lstig = Set.empty }

    member this.Attributes =
        this.Variables
        |> List.filter (fun v ->
            match v.Location with
            | I -> true
            | _ -> false)

    member this.Pcs =
        this.Sts
        |> Set.map (fun t -> t.Entry)
        |> Set.map (Map.mapValues List.singleton)
        |> Set.fold (Map.unionWith (fun _ -> List.append)) Map.empty
        |> Map.mapValues Set.ofList

    member this.LstigVariables(table: SymbolTable) =
        table.Variables
        |> Map.filter (fun _ v ->
            match v.Location with
            | L(s, _) when this.Lstig.Contains s -> true
            | _ -> false)
        |> Map.values
        |> Seq.sortBy table.M.IndexOf

and SymbolTable =
    { Spawn: Map<string, int * int>
      Agents: Map<string, AgentTable>
      Externs: Map<string, int>
      Stigmergies: Map<string, Link<Var<int> * int>>
      Processes: Map<string, Process<Var<int> * int>>
      Variables: Map<string, Var<int>>
      [<JsonIgnore>]
      M: Mapping
      [<JsonIgnore>]
      Guards: Map<Node<Stmt<Var<int> * int>>, Set<BExpr<Var<int> * int, unit>>>
      Properties: Map<string, Node<Property<Var<int> * int>>>
      Assumes: Map<string, Node<Property<Var<int> * int>>>
      Info: string option }

    static member empty =
        { Spawn = Map.empty
          Agents = Map.empty
          Externs = Map.empty
          Stigmergies = Map.empty
          Processes = Map.empty
          Variables = Map.empty
          M = Mapping.empty
          Guards = Map.empty
          Properties = Map.empty
          Assumes = Map.empty
          Info = None }



module SymbolTable =
    let internal mapVar (v: Var<_>) table =
        zero { table with M = table.M.Mapvar v }

    let private processVar externs vardef =
        let toVarInt var =
            { Location = var.Location
              Name = var.Name
              Vartype =
                match var.Vartype with
                | Array e -> Array(List.map evalCexprNoId e)
                | Scalar -> Scalar
                | C1Ref -> C1Ref
                | C2Ref -> C2Ref }

        map (VarExterns.replaceExterns externs) vardef
        |> map toVarInt
        |> fun x ->
            match x.Def.Vartype with
            | Array s when List.reduce (*) s <= 0 ->
                raise (
                    LabsException
                        { What = NonPositiveArraySize vardef.Name
                          Where = [ vardef.Pos ] }
                )
            | _ -> x

    let internal tryAddVar externs (vardef: Node<Var<Expr<unit, unit>>>) table =
        if table.Variables.ContainsKey vardef.Name then
            raise (
                LabsException
                    { What = Generic $"Unexpected operation on variable {vardef.Name}"
                      Where = [ vardef.Pos ] }
            )
        else
            zero
                { table with
                    Variables = table.Variables.Add(vardef.Name, (processVar externs vardef).Def) }


    /// Basic function to retrieve the mapping of variable named k
    let rec public findString locals table (k: string) =
        if k = "c1" || k = "c2" then
            let typ =
                match k with
                | "c1" -> C1Ref
                | _ -> C2Ref

            { Name = k
              Vartype = typ
              Location = Local },
            0
        elif Map.containsKey k locals then
            let _, loc = locals[k]

            let vtype =
                match loc with
                | Pick(n, _, _) -> Array [ n ]
                | _ -> Scalar

            { Name = k
              Vartype = vtype
              Location = loc },
            0
        elif Map.containsKey $"{k}[]" locals then
            findString locals table $"{k}[]"
        else
            table.M.TryFind k
            |> function
                | Some x -> x
                | None -> raise (LabsException { What = UndefRef k; Where = [] })

    let findAgent table name pos =
        match Map.tryFind name table.Spawn, Map.tryFind name table.Agents with
        | Some(start, bound), Some y -> start, bound, y
        | _ ->
            raise (
                LabsException
                    { What = UndefAgent name
                      Where = [ pos ] }
            )

    let private toVarRef f r o of_ =
        { Var = f r.Var
          Offset = o
          OfAgent = of_ }

    let private toVarExpr f e = Expr.map id (toVarRef f) e

    let private foreachBindings bexpr =
        let rec collect bexpr =
            match bexpr with
            | ForEach(var, _, body) ->
                let bound =
                    getRefs var
                    |> Set.map (fun v -> string v.Var, (0, Local))
                    |> Map.ofSeq

                Map.union bound (collect body)
            | Compound(_, bexprs) -> List.map collect bexprs |> List.fold Map.union Map.empty
            | _ -> Map.empty

        collect bexpr

    let internal toVarBExpr f b = BExpr.map BLeaf (toVarExpr f) b


    let private translateSub (sub: Map<_, _>) =
        let propId name =
            match sub.TryFind name with
            | Some e -> e
            | None -> failwithf $"Undefined agent {name}"

        let rec doExpr =
            function
            | Leaf f -> Leaf f
            | Ref r when sub.ContainsKey(string r.Var) -> propId (string r.Var)
            | Ref r ->
                Ref
                    { r with
                        Offset = Option.map (List.map doExpr) r.Offset
                        OfAgent = Option.map doExpr r.OfAgent }
            | Arithm(e1, op, e2) -> Arithm(doExpr e1, op, doExpr e2)
            | Unary(op, e) -> Unary(op, doExpr e)
            | Nondet(e1, e2, p) -> Nondet(doExpr e1, doExpr e2, p)
            | IfElse(c, tt, ff) -> IfElse(c, doExpr tt, doExpr ff)
            | RawCall(n, args) -> RawCall(n, List.map doExpr args)
            | QB _ -> failwith "Unexpected: nested QB"
            | Count _ -> failwith "Unexpected: nested Count"

        BExpr.map BLeaf doExpr

    let private QBToIfElse table quants pred =
        let rec trProp subs (qs: Map<_, _>) pr =
            let trQuantifier =
                function
                | All -> Conj
                | Exists -> Disj

            if not qs.IsEmpty then
                let nextId = Map.pick (fun k _ -> Some k) qs

                let agent, quantifier = qs[nextId]
                let amin, amax = table.Spawn[agent]
                let addToSubs i = Map.add nextId (Leaf(Const i)) subs
                let translateWithSubs s = trProp s (qs.Remove nextId) pr

                [ amin .. amax - 1 ]
                |> List.map (addToSubs >> translateWithSubs)
                |> fun l -> Compound(trQuantifier quantifier, l)
            else
                (translateSub subs) pr

        trProp Map.empty quants pred |> fun b -> IfElse(b, Leaf(Const 1), Leaf(Const 0))


    let private CountToSum table typ name bexpr =
        let spawn =
            match table.Spawn.TryFind typ with
            | Some x -> x
            | _ -> failwith $"Undefined agent {typ}"

        [ fst spawn .. snd spawn - 1 ]
        |> List.map (fun id -> translateSub (Map.add name (Leaf <| Const id) Map.empty) bexpr)
        |> List.map (fun e -> QBToIfElse table Map.empty e)
        |> function
            | [] -> (Const 0 |> Leaf)
            | l -> List.reduce (fun e1 e2 -> Arithm(e1, Plus, e2)) l

    let private toVarProcess table proc =
        let toVarBase f b =
            let toVarAct locals a =
                let newupdates =
                    a.Updates
                    |> List.map (fun (r, e) ->
                        let e1 =
                            match e with
                            | QB(qs, p) -> QBToIfElse table qs p
                            | Count(typ, name, expr) -> CountToSum table typ name expr
                            | _ -> e

                        toVarRef
                            (f locals)
                            r
                            (Option.map (List.map (toVarExpr (f locals))) r.Offset)
                            (Option.map (toVarExpr (f locals)) r.OfAgent),
                        toVarExpr (f locals) e1)

                { ActionType = a.ActionType
                  Updates = newupdates }

            match b.Def with
            | Act a -> toVarAct Map.empty a |> Act
            | Block b ->
                let locals =
                    let isLocal =
                        function
                        | Local
                        | Pick _ -> true
                        | _ -> false

                    List.mapi
                        (fun i act ->
                            if isLocal act.ActionType then
                                Some(List.map (fst >> fun r -> r.Var, (i, act.ActionType)) act.Updates)
                            else
                                None)
                        b
                    |> List.choose id
                    |> List.concat
                    |> Map.ofSeq

                List.map (toVarAct locals) b |> Block
            | Nil -> Nil
            | Skip -> Skip
            | Name s -> Name s
            |> fun def' ->
                { Def = def'
                  Pos = b.Pos
                  Name = b.Name
                  Source = b.Source }
            |> BaseProcess

        let fs locals = findString locals table
        let toVarBExprForeach b = toVarBExpr (fs (foreachBindings b)) b
        Process.map (toVarBase fs) toVarBExprForeach proc

    let private handleProcessNode externs table p =
        map
            (Process.simplify
             >> Process.fixGuardedPar
             >> toVarProcess table
             >> ProcessExterns.replaceExterns externs)
            p

    /// <summary>Builds a map from actions to guards.</summary>
    /// <remarks>This function assumes that all occurrences of the form
    /// (guard -> Par(...))
    /// have been transformed into
    /// (guard -> Seq([Skip; Par(...)).
    /// </remarks>
    let private setGuards proc =
        let baseFn (guards, acc) b = guards, Map.add b guards acc
        let guardFn (guards, acc) g = Set.add g guards, acc

        let compFn typ recurse (guards, acc) (l: List<_>) =
            if l.IsEmpty then
                (guards, acc)
            else
                match typ with
                | Seq ->
                    // Guards only affect the first process in a sequence
                    let _, acc' = recurse (guards, acc) l.Head
                    recurse (Set.empty, acc') (Comp(Seq, l.Tail))
                | _ ->
                    Seq.map (recurse (guards, acc)) l
                    |> Seq.reduce (fun (_, a1) (_, a2) -> Set.empty, Map.union a2 a1)

        Process.fold baseFn guardFn (fun _ acc _ -> acc) compFn (Set.empty, Map.empty) proc
        |> snd

    let expandForEach extractVar bexpr =
        match bexpr with
        | ForEach(var, arr, bExpr) ->
            let arrRef =
                match arr with
                | Ref r -> r
                | _ -> failwith $"Unexpected var {arr} in {tFOREACH}"

            let arrVar = extractVar arrRef.Var

            let dims =
                match arrVar.Vartype with
                | Array l -> l
                | _ -> failwith $"Scalar {arr} used as Array"

            let indexes =
                dims
                |> List.map (fun i -> [ 0 .. i - 1 ] |> List.map (Const >> Leaf))
                |> List.cartesian
                |> List.sort

            let refFn newOffset v off ofa =
                let varRef =
                    match var with
                    | Ref r -> extractVar r.Var
                    | _ -> failwith $"Unexpected var {var} in {tFOREACH}"

                let vref = extractVar v

                if vref.Name = varRef.Name then
                    Ref
                        { Var = arrRef.Var
                          Offset = newOffset
                          OfAgent = ofa }
                else
                    Ref { Var = v; Offset = off; OfAgent = ofa }

            let rec fexpr newOffset exp =
                cata
                    Leaf
                    (fun op e1 e2 -> Arithm(e1, op, e2))
                    (curry Unary)
                    (curryN Nondet)
                    (refFn newOffset)
                    (curry RawCall)
                    (fun c t f -> IfElse(fbexpr newOffset c, t, f))
                    exp

            and fbexpr newOffset bexp = BExpr.map BLeaf (fexpr newOffset) bexp

            indexes
            |> List.map (fun i -> fbexpr (Some i) bExpr)
            |> fun clauses -> Compound(Conj, clauses)
        | _ -> bexpr

    let private doBExpr externs table bexpr =
        let boundVar =
            match bexpr with
            | ForEach(var, _, _) -> getRefs var |> Set.map (fun v -> fst v.Var, (snd v.Var, Local)) |> Map.ofSeq
            | _ -> Map.empty

        bexpr
        |> (BExprExterns.replaceExterns externs
            >> toVarBExpr (fun (x, y) -> findString boundVar table x, y)
            >> expandForEach (fst << fst))

    let internal tryAddProcess externs (p: Node<Process<_>>) table =
        let p' = handleProcessNode externs table p
        let guards = setGuards p'.Def |> Map.mapValues (Set.map (expandForEach fst))

        zero
            { table with
                Processes = Map.add p.Name p'.Def table.Processes
                Guards = Map.union table.Guards guards }

    let internal tryAddStigmergy externs (s: Node<Stigmergy<string>>) table =
        let link =
            map
                (BExprExterns.replaceExterns externs
                 >> toVarBExpr (fun (x, y) -> (findString Map.empty) table x, y)
                 >> expandForEach (fst << fst))
                s.Def.Link

        zero
            { table with
                Stigmergies = table.Stigmergies.Add(s.Name, link.Def) }

    let internal tryAddIface externs (a: Node<Agent>) table =
        let iface = List.map (processVar externs >> (fun x -> x.Def)) a.Def.Iface

        fold mapVar iface table
        <~> fun t ->
            zero
                { t with
                    Agents =
                        table.Agents.Add(
                            a.Name,
                            { AgentTable.empty with
                                Variables = iface |> List.sortBy t.M.IndexOf }
                        ) }

    let internal tryAddAgent externs (a: Node<Agent>) (table, state) =
        List.map ((handleProcessNode externs table) >> (fun node -> node.Name, node.Def)) a.Def.Processes
        |> (Map.ofList >> zero)
        <~> fun p ->
            let allProcesses = Map.union p table.Processes

            let baseProcess =
                match p["Behavior"] with
                | BaseProcess x ->
                    match x.Def with
                    | Name x when allProcesses.ContainsKey x -> x
                    | _ -> "Behavior"
                | _ -> "Behavior"

            let otherAgent =
                match baseProcess with
                | "Behavior" -> None
                | x -> table.Agents |> Map.values |> Seq.tryFind (fun v -> v.Behavior = x)

            let p' = Map.add "Behavior" (Process.expand allProcesses "Behavior") allProcesses

            let lts, acc, init =
                match otherAgent with
                | None ->
                    let (lts, acc), initCond = makeTransitions state p'[baseProcess]
                    let lts' = removeNames p' lts
                    lts', acc, initCond
                | Some a -> a.Sts, snd state, a.InitCond

            let guards =
                Map.union table.Guards (setGuards p'["Behavior"] |> Map.mapValues (Set.map (expandForEach fst)))

            let agent =
                { table.Agents[a.Name] with
                    Behavior = baseProcess
                    Processes = p'
                    Sts = lts
                    InitCond = init
                    Lstig = a.Def.Lstig |> Set.ofList }

            zero (
                { table with
                    Agents = table.Agents.Add(a.Name, agent)
                    Guards = guards },
                (Set.empty, acc)
            )

    let internal makeSpawnRanges externs spawn table =
        let makeRanges mp =
            mp
            |> Map.fold (fun (c, m) name d -> let c' = c + d.Def in (c', (Map.add name (c, c') m))) (0, Map.empty)
            |> snd

        let spawn' =
            spawn
            |> List.map (fun (d: Node<_>) -> d.Name, d)
            |> Map.ofList
            |> Map.mapValues (map (ExprExterns.replaceExterns externs >> evalCexprNoId))

        let valid, others = Map.partition (fun _ d -> d.Def > 0) spawn'
        let zeroes, negatives = Map.partition (fun _ d -> d.Def = 0) others

        let warnings =
            zeroes
            |> Map.mapValues (fun d ->
                { What = SpawnZero d.Name
                  Where = [ d.Pos ] })
            |> Map.values

        let errors =
            negatives
            |> Map.mapValues (fun d ->
                { What = NegativeSpawn d.Name
                  Where = [ d.Pos ] })
            |> Map.values

        wrap
            { table with
                SymbolTable.Spawn = makeRanges valid }
            (List.ofSeq warnings)
            (List.ofSeq errors)

    let doProp fn p =
        let doModality =
            let doScope =
                function
                | Between(openScope, closeScope) ->
                    Between((over _predicate fn) openScope, over _predicate fn closeScope)
                | FromUntil(openScope, closeScope) ->
                    FromUntil((over _predicate fn) openScope, over _predicate fn closeScope)

            function
            | ThereIs scope -> ThereIs(doScope scope)
            | Globally scope -> Globally(doScope scope)
            | Precedes(scope, prec) -> Precedes(doScope scope, over _predicate fn prec)
            | Always -> Always
            | Eventually -> Eventually
            | Fairly -> Fairly
            | FairlyInf -> FairlyInf
            | Finally -> Finally

        { QuantPredicate = over _predicate fn p.QuantPredicate
          Name = p.Name
          Modality = doModality (view _modality p) }

    let internal tryAddProperty externs (p: Node<Property<string>>) (table: SymbolTable) =
        let fn = doBExpr externs table

        zero
            { table with
                Properties = Map.add p.Name (map (doProp fn) p) table.Properties }

    let internal tryAddAssume externs (p: Node<Property<string>>) (table: SymbolTable) =
        let fn = doBExpr externs table

        zero
            { table with
                Assumes = Map.add p.Name (map (doProp fn) p) table.Assumes }

    let lstigVariablesOf table name =
        table.Variables
        |> Map.filter (fun _ v ->
            match v.Location with
            | L(s, _) when table.Agents[name].Lstig.Contains s -> true
            | _ -> false)
        |> Map.values
        |> Seq.sortBy table.M.IndexOf

    let private maybeFilterProp prop m =
        match prop with
        | Some p ->
            let m' = Map.filter (fun k _ -> k = p) m

            if m'.IsEmpty then
                failwith $"Property {p} not found."
            else
                ()

            m'
        | None -> m

    let dump (table: SymbolTable) prop =
        let dumpSource p =
            p.Source
            |> _.Replace('\n', ' ')
            |> fun s -> let i = s.IndexOf('=') in s.Substring(i + 1).Trim() // Remove property name
            |> fun s -> Regex.Replace(s, "\s+", " ") // Remove duplicate spaces

        let dumpVar v =
            match v.Vartype with
            | Scalar -> $"%i{table.M.IndexOf v}={v.Name}"
            | C1Ref -> "c1"
            | C2Ref -> "c2"
            | Array s -> $"%i{snd table.M[v.Name]}={v.Name}[%i{List.reduce (*) s}]"

        let dumpSpawn agentName (_start, _end) =
            let iface =
                table.Agents[agentName].Variables |> List.map dumpVar |> String.concat ";"

            let lstig =
                table.Agents[agentName].LstigVariables table
                |> Seq.map dumpVar
                |> String.concat ";"

            $"{agentName} %i{_start},%i{_end}\n{iface}\n{lstig}"

        let dumpPicks agentName =
            let picks =
                table.Agents[agentName].Processes["Behavior"]
                |> Process.collectPicks
                |> String.concat ","

            $"{agentName} {picks};"


        let s1 =
            table.Variables
            |> Map.filter (fun _ -> isEnvVar)
            |> Map.values
            |> Seq.sortBy table.M.IndexOf
            |> Seq.map dumpVar
            |> String.concat ";"

        let s2 = Map.map dumpSpawn table.Spawn |> Map.values |> String.concat "\n"

        let s3 =
            table.Properties
            |> maybeFilterProp prop
            |> Map.mapValues dumpSource
            |> Map.values
            |> String.concat ";"

        let s4 =
            table.Assumes |> Map.mapValues dumpSource |> Map.values |> String.concat ";"

        let s5 =
            Map.map (fun k _ -> dumpPicks k) table.Spawn |> Map.values |> String.concat ""

        String.concat "\n" [ s1; s2; s3; s4; s5 ]

type SymbolTable with
    member this.Dump(prop) = SymbolTable.dump this prop

    member this.TranslateBExpr(bexpr) =
        (BExprExterns.replaceExterns this.Externs
         >> SymbolTable.toVarBExpr (fun (x, y) -> SymbolTable.findString Map.empty this x, y)
         >> SymbolTable.expandForEach (fst << fst))
            bexpr
