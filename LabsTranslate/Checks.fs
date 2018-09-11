module Checks
open Types
open Link
open Base

/// Verifies that all process names in the program have been defined.
let checkNames sys =
    let globalNames = Map.keys sys.processes

    let rec usedNames = function
        | Name(n) -> Set.singleton n
        | Await(_, p) -> usedNames p
        | Seq(p, q)
        | Par(p, q)
        | Choice(p, q) -> Set.union (usedNames p) (usedNames q)
        | _ -> Set.empty

    let undefinedNames processes defNames = 
        processes
        |> Map.values
        |> Seq.map usedNames
        |> Set.unionMany
        |> fun x -> Set.difference x defNames

    let checkComps = 
        sys.components
        |> Map.map (fun _ x -> 
            undefinedNames x.processes (Set.union globalNames (Map.keys x.processes)))
        |> Map.filter (fun _ undef -> undef.Count > 0)
    let globalUndef = undefinedNames sys.processes globalNames    

    let rec makeMsg globalCount localCount = 
        match globalCount, localCount with
        | (true,true) -> ""
        | (_,true) -> sprintf "global: the following processes are undefined: %s" (withcommas globalUndef)
        | (true,_) -> 
            checkComps
            |> Map.map (fun name undefs ->
                sprintf "%s: the following processes are undefined: %s" name (withcommas undefs))
            |> Map.values
            |> String.concat "\n"
        | (_,_) -> (makeMsg  true false) + "\n" + (makeMsg false true)

    let msg = makeMsg globalUndef.IsEmpty checkComps.IsEmpty
    if msg = "" then Result.Ok sys else Result.Error msg

let checkComponents sys =
    let isDefined (def:ComponentDef<'a>) name  =
        sys.processes.ContainsKey name || def.processes.ContainsKey name

    let undefBehaviors = 
        sys.components
        |> Map.filter (fun _ def -> not <| isDefined def def.behavior)

    if sys.components.IsEmpty then
        Result.Error "No components defined"
    else
        if undefBehaviors.IsEmpty then
            Result.Ok sys
        else
            undefBehaviors
            |> Map.map (fun _ def -> def.behavior)
            |> Map.map (sprintf "%s: Behavior is undefined: %s")
            |> Map.values
            |> withcommas
            |> Result.Error

let resolveSystem (sys:SystemDef<string>, mapping:KeyMapping) =
    let findVar name = getInfoOrFail mapping name |> fst

    let resolveLinkTerm = function
        | RefC1 k -> findVar k |> RefC1
        | RefC2 k -> findVar k |> RefC2
        
    let rec toVarExpr finder = function
        | Expr.Const i -> Const i
        | Expr.Ref (r, offset) -> 
            Expr.Ref(finder r, Option.map (toVarExpr finder) offset)
        | Expr.Arithm(e1, op, e2) ->
            Expr.Arithm(toVarExpr finder e1, op, toVarExpr finder e2)

    let rec toVarBExpr finder = function
    | BExpr.True -> BExpr.True
    | BExpr.False -> False
    | BExpr.Compare(e1, bop, e2) ->
        BExpr.Compare(toVarExpr finder e1, bop, toVarExpr finder e2)
    | BExpr.Neg b -> toVarBExpr finder b |> BExpr.Neg
    | BExpr.Conj(b1, b2) ->
        BExpr.Conj(toVarBExpr finder b1, toVarBExpr finder b2)

    let resolveAction action =
        let r, o, expr, expectedLoc, actionType = 
            match action with
            | AttrUpdate(r, o, expr) -> r, o, expr, I, AttrUpdate
            | LStigUpdate(r, o, expr) -> r, o, expr, L, LStigUpdate
            | EnvWrite(r, o, expr) -> r, o, expr, E, EnvWrite
        let info, _ = getInfoOrFail mapping r
        if info.location <> expectedLoc then 
            sprintf "%A variable %s, treated as %A" info.location r expectedLoc
            |> failwith
        else
            actionType(
                info,
                Option.map (toVarExpr findVar) o,
                toVarExpr findVar expr)

    let rec resolveProcess = function
    | Nil -> Nil
    | Skip -> Skip
    | Base(a) -> resolveAction a |> Base
    | Await(b, p) -> Await(toVarBExpr findVar b, resolveProcess p)
    | Seq(p1, p2) -> Seq(resolveProcess p1, resolveProcess p2)
    | Choice(p1, p2) -> Choice(resolveProcess p1, resolveProcess p2)
    | Par(p1, p2) -> Par(resolveProcess p1, resolveProcess p2)
    | Name(n) -> Name n

    let resolveComponent (c:ComponentDef<string>) = {
        name = c.name
        behavior = c.behavior
        iface = c.iface
        lstig = c.lstig
        processes = c.processes |> Map.mapValues resolveProcess
    }

    let resolveProp (pr:Property<string>) = 
        let find (name, otherStuff) = 
            findVar name, otherStuff
        {
            name=pr.name
            predicate=(toVarBExpr find pr.predicate)
            modality=pr.modality
            quantifiers=pr.quantifiers
        }

    ({
        components = sys.components |> Map.mapValues resolveComponent
        environment = sys.environment 
        processes = sys.processes |> Map.mapValues resolveProcess
        spawn = sys.spawn
        properties = sys.properties |> Map.mapValues resolveProp
        link = sys.link |> (toVarBExpr resolveLinkTerm)
    }, mapping) |> Result.Ok

let analyzeKeys sys = 
    let comps = Map.values sys.components

    let conflicts setOfVars =
        let hasSameName (v1:Var) (v2:Var) =
            v1.ToString() = v2.ToString()
        setOfVars
        |> Set.map (fun x -> (x.ToString(), Set.filter (hasSameName x) setOfVars))
        |> Set.filter (fun (name, number) -> number.Count > 1)
        |> fun x -> 
            if x.IsEmpty 
            then Result.Ok setOfVars
            else 
                x 
                |> Set.map fst
                |> withcommas
                |> sprintf "Duplicate variable definitions for %s"
                |> Result.Error

    /// Makes a dictionary with information about each 
    /// variable in setOfVars.
    let makeInfo location startFrom setOfVars = 
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
        |> Result.map (makeInfo I 0)
        |> Result.map fst

    let lstigkeys = 
        comps
        |> Seq.map (fun c -> List.map Map.keys c.lstig)
        |> Seq.map Set.unionMany
        |> Seq.fold 
            (fun (map, i) vars -> 
                let newInfo, newI = makeInfo L i vars
                (Map.merge map newInfo, newI)
            )
            (Map.empty, 0)
        |> fst

    let envKeys = 
        Map.keys sys.environment
        |> makeInfo E 0
        |> fst 

    attrKeys
    >>= Map.mergeIfDisjoint lstigkeys
    >>= Map.mergeIfDisjoint envKeys
    |> Result.map (fun m -> (sys, m))