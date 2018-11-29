module internal Checks
open Types
open Link
open Base

/// Verifies that all process names in the program have been defined.
let checkNames sys =

    let rec usedNames = function
        | Name (n, _) -> Set.singleton n
        | Await(_, p) -> usedNames p
        | Seq(p, q)
        | Par(p, q)
        | Choice(p, q) -> Set.union (usedNames p) (usedNames q)
        | _ -> Set.empty

    let undefinedNames processes = 
        let defNames = Map.keys processes |> Set.union (Map.keys sys.processes)
        processes
        |> Map.values
        |> Seq.map usedNames
        |> Set.unionMany
        |> fun x -> Set.difference x defNames

    let checkComps = 
        sys.components
        |> Map.mapValues (fun x -> undefinedNames x.processes)
        |> Map.filter (fun _ undef -> undef.Count > 0)

    let localResult initialmsg =
        checkComps
        |> Map.fold (fun msg name undefs ->
            if undefs.IsEmpty then msg else 
                sprintf 
                    "%s\n%s: the following processes are undefined: %s"
                    msg name (withcommas undefs)
        ) initialmsg
        |> fun msg -> if msg.Length = 0 then Ok () else Error msg

    undefinedNames sys.processes
    |> fun x -> 
        if x.IsEmpty
        then ""
        else sprintf "global: the following processes are undefined: %s" (withcommas x)
    |> localResult

/// Performs several checks related to components
let checkComponents sys =

    let undefBehaviors = 
        sys.components
        |> Map.filter (fun _ def -> not <| def.processes.ContainsKey "Behavior")

    if sys.components.IsEmpty then
        Result.Error "No components defined"
    else
        if undefBehaviors.IsEmpty then
            Result.Ok ()
        else
            undefBehaviors
            |> Map.map (fun name _ -> sprintf "%s: Behavior is undefined" name)
            |> Map.values
            |> String.concat "\n"
            |> Result.Error
            
/// Binds all references in the system to the corresponding variable.
let resolveSystem (sys:SystemDef<string>, mapping:KeyMapping) =
    
    let addUndefs (sys:SystemDef<'a>) (cmp:ComponentDef<'a>) =
        sys.ifaceVars.Value
        |> Set.filter (fun v -> 
            cmp.iface |> Set.forall (fun x -> v.name <> x.name))
        //|> Map.mapValues (fun v -> {v with init=Undef})
        |> fun undefs -> {cmp with iface = Set.union cmp.iface undefs}

    let newComps =
        sys.components
        |> Map.mapValues (addUndefs sys)

    let allVars =
        sys.ifaceVars.Value 
        |> Set.union sys.lstigVars.Value
        |> Set.union sys.environment
        |> Set.map (fun v -> v.name, v)
        |> Map.ofSeq
         
    let findString k = 
        match allVars.TryFind k with
        | Some v -> v, mapping.[v.name]
        | None -> failwith (sprintf "Undefined variable: %s" k)

    let rec toVarExpr finder = function
        | Expr.Id i -> Expr.Id i
        | Const i -> Const i
        | Abs e -> Abs (toVarExpr finder e)
        | Ref r -> Ref (toVarRef finder r)
        | Arithm(e1, op, e2) ->
            Expr.Arithm(toVarExpr finder e1, op, toVarExpr finder e2)
    and toVarRef finder r = 
        {var= finder r.var; offset=Option.map (toVarExpr finder) r.offset}

    let rec toVarBExpr finder = function
    | True -> True
    | False -> False
    | Compare(e1, bop, e2) ->
        Compare(toVarExpr finder e1, bop, toVarExpr finder e2)
    | Neg b -> toVarBExpr finder b |> Neg
    | Compound(b1, op, b2) ->
        Compound(toVarBExpr finder b1, op, toVarBExpr finder b2)

    let resolveAction action =
        let locationCheck expectedLoc r =
            let info = allVars.[r.var]
            match expectedLoc with
            | L _ -> 
                match info.location with L _ -> true | _ -> false
            | _ -> expectedLoc = info.location
            
        action.updates
        |> List.map fst
        |> List.filter (not << (locationCheck action.actionType))
        |> List.map (fun r ->
            let info = allVars.[r.var]
            sprintf "%O variable %s, treated as %O" info.location r.var action.actionType)
        |> fun x -> 
            if not x.IsEmpty then 
                x |> String.concat "\n" |> failwith
            else
                action.updates
                |> List.map (fun (r, e) ->
                        toVarRef findString r, toVarExpr findString e)
                |> fun x -> {actionType=action.actionType; updates=x}

    let rec resolveProcess = function
    | Nil -> Nil
    | Skip pos -> Skip pos
    | Base (a, pos) -> Base(resolveAction a, pos)
    | Await(b, p) -> Await(toVarBExpr findString b, resolveProcess p)
    | Seq(p1, p2) -> Seq(resolveProcess p1, resolveProcess p2)
    | Choice(p1, p2) -> Choice(resolveProcess p1, resolveProcess p2)
    | Par(p1, p2) -> Par(resolveProcess p1, resolveProcess p2)
    | Name (n, pos) -> Name (n, pos)

    let resolveComponent (c:ComponentDef<string>) = {
        name = c.name
        iface = c.iface
        lstig = c.lstig
        processes = c.processes |> Map.mapValues resolveProcess
    }

    let resolveStigmergy lstig =
        {
            name = lstig.name
            link = lstig.link |> (toVarBExpr (fun (x, y) -> findString x, y))
            vars = lstig.vars
        }

    let resolveProp (pr:Property<string>) = 
        {
            name=pr.name
            predicate=pr.predicate |> (toVarBExpr (fun (x, y) -> findString x, y))
            modality=pr.modality
            quantifiers=pr.quantifiers
        }

    ({
        components = newComps |> Map.mapValues resolveComponent
        stigmergies = sys.stigmergies |> Map.mapValues resolveStigmergy
        environment = sys.environment 
        processes = sys.processes |> Map.mapValues resolveProcess
        spawn = sys.spawn
        properties = sys.properties |> Map.mapValues resolveProp
    }) |> Result.Ok

let analyzeKeys sys = 
    let comps = Map.values sys.components

    let conflicts (seqOfVars:Set<Var> seq) =
        let groups =
            seqOfVars
            |> Set.unionMany
            |> List.ofSeq
            |> List.groupBy (fun x -> x.name)
            //|> Set.unionMany
            //|> Seq.map (fun k -> k, (seqOfVars |> Seq.choose (Map.tryFind k) |> List.ofSeq))
            |> Map.ofSeq

        groups
        |> Map.mapValues (fun l -> 
            l,
            if l.IsEmpty then false
            else List.exists (fun v -> v.vartype <> l.Head.vartype) l)
        |> Map.filter (fun _ -> snd)
        |> Map.keys
        |> fun x -> 
            if x.IsEmpty then seqOfVars else 
                x
                |> Set.map (sprintf "Variable %s cannot be both Scalar and Array") 
                |> String.concat "\n"
                |> failwith

    /// Makes a dictionary with information about each 
    /// variable in setOfVars.
    let makeInfo startFrom (setOfVars:Set<Var> seq) = 
        let update (mapping, nextIndex) (var:Var) =
            if Map.containsKey var.name mapping
            then mapping, nextIndex
            else 
                Map.add var.name nextIndex mapping,
                match var.vartype with
                | Scalar _ -> nextIndex + 1
                | Array n -> nextIndex + n

        setOfVars
        |> Seq.fold (fun x s -> Set.fold update x s) (Map.empty, startFrom)

    let attrKeys, maxI = 
        comps
        |> Seq.map (fun c -> c.iface)
        |> conflicts
        |> (makeInfo 0)

    let lstigkeys, maxL =
        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.fold 
            (fun (map, i) vars -> 
                let newInfo, newI = makeInfo i (seq vars)
                (Map.merge map newInfo, newI)
            )
            (Map.empty, 0)

    let envKeys, maxE = 
        sys.environment
        |> Seq.singleton
        |> makeInfo 0

    attrKeys
    |> Map.mergeIfDisjoint lstigkeys
    |> Map.mergeIfDisjoint envKeys
    |> Ok
    >+> Ok (max maxI 1, max maxL 1, max maxE 1)