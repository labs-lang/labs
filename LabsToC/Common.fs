module LabsToC.Common
open Frontend
open LabsCore
open Types

let private refTypeCheck v (offset:'a option) =
    // TODO move to frontend
    let test, msg = 
        match v.vartype with
        | Scalar -> offset.IsSome, (sprintf "Scalar %s treated as Array")
        | Array _ -> offset.IsNone, (sprintf "Array %s treated as Scalar")
    if test then failwith (msg v.name) else ()

/// Returns the set of all stigmergy variables accessed by the expression.
let internal getLstigVars expr =
    Expr.getVars expr
    |> Set.filter (fun (v, _) -> isLstigVar v)

/// Translates a variable reference.
let internal trref trLocation name (v:Var<int>, i:int) offset =
    do refTypeCheck v offset
    let index =
        match offset with
        | None -> string i
        | Some off -> sprintf "%i + %s" i off
    (trLocation v.location) name index

let internal translateBExpr bleaf_ neg_ compound_ filter trExpr bexpr =
    let undefs =
        match filter with
        | None -> Set.empty
        | Some f ->
            let undef_ expr =
                Set.filter f (Expr.getRefs expr)
                |> Set.map (fun r -> Compare(Ref(r), Neq, Leaf(Extern "undef_value")))
            BExpr.cata (fun _ -> Set.empty) id (fun _ e1 e2 -> Set.union (undef_ e1) (undef_ e2)) (fun _ -> Set.unionMany) bexpr
    
    let compare_ op e1 e2 = sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2) //TODO
        
    if undefs.IsEmpty then bexpr
    else
        Set.add bexpr undefs
        |> fun s -> Compound(Conj, s |> Set.toList)
//        |> Seq.reduce (fun b1 b2 -> Compound(b1, Conj, b2))
    |> BExpr.cata bleaf_ neg_ compare_ compound_
    
/// Build consistent Expr/BExpr translators using trRef and trId. 
let internal makeTranslators trExpr trBexpr trRef trId =
    let expr = trExpr trRef trId
    expr, trBexpr expr

let translateProp trExpr trBExpr trLocation (table:SymbolTable) (p:Node<Property<_>>) =
    let ex = Map.exists (fun _ (_, q)-> q = Exists) p.def.quantifiers
    let fa = Map.exists (fun _ (_, q)-> q = All) p.def.quantifiers
    
    //TODO move checks to frontend
    let translateSub (sub:Map<_,_>) =
        let propId name =
            match sub.TryFind name with
            | Some e -> e
            | None -> failwithf "Undefined agent %s" name
        
        let propRef1 ((v:Var<_>, i), c) offset =
            match c with
            | None -> 
                if v.location <> E then failwithf "%s is not an environment variable" v.name
                {var=((v, i), c); offset=offset}
            | Some c ->
                {var=((v, i), (Some <| (string << propId) c)); offset=offset}

        BExpr.map (BLeaf) (Expr.map id (fun r o -> propRef1 r.var o))
        
    if (ex && fa) then 
        p.name
        |> failwithf "Property %s: alternating quantifiers are currently not supported"

    let rec trProp subs prop =
        let trQuantifier = function | All -> Conj | Exists -> Disj
        if not prop.quantifiers.IsEmpty then
            let nextId = Map.pick (fun k _ -> Some k) prop.quantifiers
            let agent, quantifier = prop.quantifiers.[nextId]
            let amin, amax = table.spawn.[agent]

            [amin..amax-1]
            |> List.map (fun i -> Map.add nextId i subs)
            |> List.map (fun s -> trProp s {prop with quantifiers=prop.quantifiers.Remove nextId})
            |> fun l -> Compound(trQuantifier quantifier, l)
            |> BExpr.simplify
            
//            |> List.reduce (fun b1 b2 -> Compound(b1, trQuantifier quantifier, b2))
//            |> String.concat (trQuantifier quantifier)
        else
            (translateSub subs) prop.predicate
    
    let propRef ((v:Var<_>, i), c) offset =
        match c with
        | None -> trref trLocation "" (v, i) offset
        | Some c -> (trref trLocation c (v, i) offset)
            
    trProp Map.empty p.def
    |> trBExpr (trExpr propRef id)
    
type TranslationKit = {
    agentExprTr: Expr<Var<int> * int, unit> -> string
    agentGuardTr: BExpr<Var<int> * int, unit> -> string
    mainGuardTr: BExpr<Var<int> * int, unit> -> string
    initTr: string -> BExpr<Var<int> * int, unit> -> string
    linkTr: BExpr<(Var<int> * int) * LinkComponent, LinkComponent> -> string
    propTr: SymbolTable -> Node<Property<Var<int> * int>> -> string 
}