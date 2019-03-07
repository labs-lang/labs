module internal Checks
open Types
open LabsCore
open Link
open Base
open FSharpPlus

/// Performs several checks related to components
let checkComponents sys =
    let undefBehaviors = 
        sys.components
        |> Map.filter (fun _ def -> not <| Map.containsKey "Behavior" def.processes)
    if sys.components.IsEmpty then
        Result.Error "No agents defined"
    else
        if undefBehaviors.IsEmpty then
            Result.Ok ()
        else
            undefBehaviors
            |> Map.map (fun name _ -> sprintf "%s: Behavior is undefined" name)
            |> Map.values
            |> String.concat "\n"
            |> Result.Error



/// Verifies that all process names in the program have been defined.
let checkNames sys =
    let check definitions =
        let formatErrorMsg where n =
            sprintf "%s: Undefined name %s" where n
        let isDefined n = Map.containsKey n definitions
        definitions
        |> Map.mapValues Process.usedNames
        |> Map.map (fun k used ->
            let undefs = Set.filter (not << isDefined) used
            if undefs.IsEmpty then Ok()
            else 
                undefs
                |> Set.map (formatErrorMsg k)
                |> String.concat "\n"
                |> Error
        )
        |> Map.values
        |> Result.foldErrors (sprintf "%s\n%s") ""
        |> fun x -> if x = "" then Ok() else Error x
        
    let checkAgent (a:ComponentDef<_>) =
        check (Map.union a.processes sys.processes)

    Map.mapValues checkAgent sys.components
    |> Map.values
    |> if sys.processes.IsEmpty then id else Seq.append (Seq.singleton (check sys.processes))
    |> Seq.reduceBack (fun a b -> a >>= fun _ -> b)

let analyzeKeys sys = 
    let comps = Map.values sys.components

    let conflicts (seqOfVars:Set<Var> seq) =
        let groups =
            seqOfVars
            |> Set.unionMany
            |> List.ofSeq
            |> List.groupBy (fun x -> x.name)
            |> Map.ofSeq

        groups
        |> Map.mapValues (fun l -> 
            l,
            if l.IsEmpty then false
            else List.exists (fun v -> v.vartype <> l.Head.vartype) l)
        |> Map.filter (fun _ -> snd)
        |> Map.keys
        |> Seq.map (sprintf "Variable %s cannot be both Scalar and Array") 
        |> String.concat "\n"
        |> fun msg -> if msg = "" then seqOfVars else failwith msg

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
                (Map.union newInfo map, newI)
            )
            (Map.empty, 0)

    let envKeys, maxE = 
        sys.environment
        |> Seq.singleton
        |> makeInfo 0
    
    let failOnDuplicate k _ _ = failwithf "Duplicate key found: %s" k
    
    attrKeys
    |> Map.unionWith failOnDuplicate lstigkeys
    |> Map.unionWith failOnDuplicate envKeys
    |> fun mapping -> Ok (sys, mapping, max maxI 1, max maxL 1, max maxE 1)

/// Binds all references in the system to the corresponding variable.
let toVarSystem (sys:SystemDef<string>) (mapping:KeyMapping) =

    let addUndefs cmp =
        let vname_ (v:Var) = v.name
        let undefs : Set<Var> = 
            let names = Set.map vname_ cmp.iface
            Set.filter (not << names.Contains << vname_) sys.ifaceVars.Value
        {cmp with iface = Set.union cmp.iface undefs}

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

    let toVarRef f r o =
        {var=f r.var; offset=o}
    let toVarExpr f e = 
        Expr.map (Leaf) (fun r o -> Ref(toVarRef f r o)) e
    
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

    let toVarBExpr f b =
        BExpr.map (BLeaf) (toVarExpr f) b

    let toVarProcess proc = 
        Process.map ((toVarBase findString) >> BaseProcess) (toVarBExpr findString) proc
        
    let toVarComponent (c:ComponentDef<string>) =
        {
            name = c.name
            iface = c.iface
            lstig = c.lstig
            processes = c.processes |> Map.mapValues toVarProcess
        }

    let toVarStigmergy lstig =
        {
            name = lstig.name
            link = lstig.link |> (toVarBExpr (fun (x,y) -> findString x, y))
            vars = lstig.vars
        }

    let toVarProperty (pr:Property<string>) = 
        {
            name=pr.name
            predicate=pr.predicate |> (toVarBExpr (fun (x,y) -> findString x, y))
            modality=pr.modality
            quantifiers=pr.quantifiers
        }
    
    {
        components =
            sys.components
            |> Map.mapValues (addUndefs >> toVarComponent)
        stigmergies = sys.stigmergies |> Map.mapValues toVarStigmergy
        environment = sys.environment 
        processes = sys.processes |> Map.mapValues toVarProcess
        spawn = sys.spawn
        properties = sys.properties |> Map.mapValues toVarProperty
    }