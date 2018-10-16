module internal Checks
open Types
open Link
open Base

/// Verifies that all process names in the program have been defined.
let checkNames sys =

    let rec usedNames = function
        | Name n -> Set.singleton n
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
    let findString k = 
        match mapping.TryFind k with
        | Some(var, _) -> var
        | None -> failwith (sprintf "Undefined variable: %s" k)

    let resolveLinkTerm = function
        | RefC1 k -> findString k |> RefC1
        | RefC2 k -> findString k |> RefC2

    let rec toVarExpr finder = function
        | Id i -> Id i
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
            let info = findString r.var
            expectedLoc = info.location
        let lstigCheck r =
            let info = findString r.var
            match info.location with
            | L _ -> true   
            | _ -> false

        let check, failMsg = 
            match action.actionType with
            | I -> locationCheck I, "attribute"
            | L _ -> lstigCheck, "stigmergy"
            | E -> locationCheck E, "environment"

        action.updates
        |> List.map fst
        |> List.filter (not << check)
        |> List.map (fun r ->
            let info = findString r.var
            sprintf "%O variable %s, treated as %s" info.location r.var failMsg
        )
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
    | Skip -> Skip
    | Base a -> resolveAction a |> Base
    | Await(b, p) -> Await(toVarBExpr findString b, resolveProcess p)
    | Seq(p1, p2) -> Seq(resolveProcess p1, resolveProcess p2)
    | Choice(p1, p2) -> Choice(resolveProcess p1, resolveProcess p2)
    | Par(p1, p2) -> Par(resolveProcess p1, resolveProcess p2)
    | Name n -> Name n

    let resolveComponent (c:ComponentDef<string>) = {
        name = c.name
        iface = c.iface
        lstig = c.lstig
        processes = c.processes |> Map.mapValues resolveProcess
    }

    let resolveStigmergy lstig =
        {
            name = lstig.name
            link = lstig.link |> (toVarBExpr resolveLinkTerm)
            vars = lstig.vars
        }

    let resolveProp (pr:Property<string>) = 
        let find (name, otherStuff) = 
            findString name, otherStuff
        {
            name=pr.name
            predicate=(toVarBExpr find pr.predicate)
            modality=pr.modality
            quantifiers=pr.quantifiers
        }

    ({
        components = sys.components |> Map.mapValues resolveComponent
        stigmergies = sys.stigmergies |> Map.mapValues resolveStigmergy
        environment = sys.environment 
        processes = sys.processes |> Map.mapValues resolveProcess
        spawn = sys.spawn
        properties = sys.properties |> Map.mapValues resolveProp
        //link = sys.link |> (toVarBExpr resolveLinkTerm)
    }, mapping) |> Result.Ok

let analyzeKeys sys = 
    let comps = Map.values sys.components

    let conflicts (setOfVars:Set<Var>) =
        let hasSameName (v1:Var) (v2:Var) =
            v1.name = v2.name
        setOfVars
        |> Set.map (fun x -> (x.name, Set.filter (hasSameName x) setOfVars))
        |> Set.filter (fun (_, vars) -> vars.Count > 1)
        |> fun x -> 
            if x.IsEmpty
            then setOfVars
            else 
                x 
                |> Set.map fst
                |> withcommas
                |> sprintf "Duplicate variable definitions for %s"
                |> failwith

    /// Makes a dictionary with information about each 
    /// variable in setOfVars.
    let makeInfo startFrom setOfVars = 
        let update (mapping, nextIndex) (var:Var) =
            let newMapping = 
                Map.add 
                    (var.name) 
                    (var, nextIndex)
                    mapping
            let newIndex =
                match var.vartype with
                | Scalar _ -> nextIndex + 1
                | Array n -> nextIndex + n
            (newMapping, newIndex)
           
        setOfVars
        |> Set.fold update (Map.empty, startFrom)
        
    let attrKeys = 
        comps
        |> Seq.map (fun c -> Map.keys c.iface)
        |> Set.unionMany
        |> conflicts
        |> (makeInfo 0)
        |> fst

    let lstigkeys =
        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.map (List.map Map.keys)
        |> Seq.map Set.unionMany //FIXME duplicate lstig keys may go unnoticed
        |> Seq.fold 
            (fun (map, i) vars -> 
                let newInfo, newI = makeInfo i vars
                (Map.merge map newInfo, newI)
            )
            (Map.empty, 0)
        |> fst 

    let envKeys = 
        Map.keys sys.environment
        |> makeInfo 0
        |> fst 

    attrKeys
    |> Map.mergeIfDisjoint lstigkeys
    |> Map.mergeIfDisjoint envKeys
    |> Result.Ok