module internal LabsToC.Common
open Frontend
open LabsCore.Grammar
open LabsCore
open Types
open System.IO

let private refTypeCheck v (offset:'a option) =
    // TODO move to frontend
    let test, msg = 
        match v.Vartype with
        | Scalar -> offset.IsSome, (sprintf "Scalar %s treated as Array")
        | Array _ -> offset.IsNone, (sprintf "Array %s treated as Scalar")
    if test then failwith (msg v.Name) else ()

/// Returns the set of all stigmergy variables accessed by the expression.
let getLstigVars expr =
    Expr.getVars expr
    |> Set.filter (fun (v, _) -> isLstigVar v)

/// Translates a variable reference.
let private trref trLocation name (v:Var<int>, i:int) offset =
    do refTypeCheck v offset //TODO move
    let index =
        match offset with
        | None -> string i
        | Some off -> sprintf "%i + %s" i off
    trLocation v.Location name index

let translateBExpr bleafFn negFn compareFn compoundFn filter bexpr =
    let undefs =
        match filter with
        | None -> Set.empty
        | Some f ->
            /// Checks that expr is "well-defined"
            /// (i.e., referenced vars must not be equal to "undef_value")
            let checkWellDef expr =
                Set.filter f (Expr.getRefs expr)
                |> Set.map (fun r -> Compare(Ref(r), Neq, Leaf(Extern "undef_value")))
            BExpr.cata (fun _ -> Set.empty) id (fun _ e1 e2 -> Set.union (checkWellDef e1) (checkWellDef e2)) (fun _ -> Set.unionMany) bexpr
        
    if undefs.IsEmpty then bexpr
    else
        Set.add bexpr undefs
        |> fun s -> Compound(Conj, s |> Set.toList)
    |> BExpr.cata bleafFn negFn compareFn compoundFn
    
let private translateProp trExpr trBExpr trLocation (table:SymbolTable) (p:Node<Property<_>>) =
    let ex = Map.exists (fun _ (_, q)-> q = Exists) p.Def.Quantifiers
    let fa = Map.exists (fun _ (_, q)-> q = All) p.Def.Quantifiers
    
    //TODO move checks to frontend
    let translateSub (sub:Map<_,_>) =
        let propId name =
            match sub.TryFind name with
            | Some e -> e
            | None -> failwithf "Undefined agent %s" name
        
        let propRef1 ((v:Var<_>, i), c) offset =
            match c with
            | None -> 
                if v.Location <> E then failwithf "%s is not an environment variable" v.Name
                {Var=((v, i), c); Offset=offset}
            | Some c ->
                {Var=((v, i), (Some <| (string << propId) c)); Offset=offset}

        let trLeaf leaf =
            match leaf with
            | Id name -> (string << propId) name |> Id
            | _ -> leaf
        
        BExpr.map (BLeaf) (Expr.map trLeaf (fun r o -> propRef1 r.Var o))
        
    if (ex && fa) then 
        p.Name
        |> failwithf "Property %s: alternating quantifiers are currently not supported"

    let rec trProp subs prop =
        let trQuantifier = function | All -> Conj | Exists -> Disj
        if not prop.Quantifiers.IsEmpty then
            let nextId = Map.pick (fun k _ -> Some k) prop.Quantifiers
            let agent, quantifier = prop.Quantifiers.[nextId]
            let amin, amax = table.spawn.[agent]

            let addToSubs i = Map.add nextId i subs
            let translateWithSubs s =
                trProp s {prop with Quantifiers=prop.Quantifiers.Remove nextId} 

            [amin..amax-1]
            |> List.map (addToSubs >> translateWithSubs)
            |> fun l -> Compound(trQuantifier quantifier, l)
            |> BExpr.simplify
        else
            (translateSub subs) prop.Predicate
    
    let propRef ((v:Var<_>, i), c) offset =
        match c with
        | None -> trref trLocation "" (v, i) offset
        | Some c -> (trref trLocation c (v, i) offset)
            
    trProp Map.empty p.Def
    |> trBExpr (trExpr propRef id)
    

type TemplateInfo = {
    BaseDir: string
    Extension: string
}
with
    member this.Get name =
        Path.Combine(this.BaseDir, name)
        |> fun path -> Path.ChangeExtension(path, this.Extension)

    
type TranslationKit = {
    AgentExprTr: Expr<Var<int> * int, unit> -> string
    AgentGuardTr: BExpr<Var<int> * int, unit> -> string
    MainGuardTr: BExpr<Var<int> * int, unit> -> string
    InitTr: Var<int> * int -> int -> string list
    LinkTr: BExpr<(Var<int> * int) * LinkComponent, LinkComponent> -> string
    PropTr: SymbolTable -> Node<Property<Var<int> * int>> -> string
    TemplateInfo : TemplateInfo
}

type RefTranslator<'a> = 'a -> string option -> string

type IWrapper =
    abstract member InitId : int -> LeafExpr<'b>
    abstract member AgentName : string
    abstract member TemplateInfo : TemplateInfo
    abstract member TrLoc<'a> : Location -> string -> 'a -> string
    abstract member TrInitLoc<'a> : Location -> string -> 'a -> string
    abstract member TrLinkId : LinkComponent -> string
    abstract member TrExpr<'a, 'b> : RefTranslator<'a> -> ('b -> string) -> Expr<'a, 'b> -> string
    abstract member TrBExpr<'a, 'b when 'a:comparison and 'b:comparison> : (Ref<'a, 'b> -> bool) option -> (Expr<'a, 'b> -> string) -> BExpr<'a, 'b> -> string
        
let makeTranslator (wrapper: IWrapper) trRef trId filter =
    let expr = wrapper.TrExpr trRef trId
    expr, wrapper.TrBExpr filter expr

let translateKit (p:IWrapper) =
    let agentExprTr = p.TrExpr (trref p.TrLoc p.AgentName) (fun () -> p.AgentName)
    let agentGuardTr = p.TrBExpr (Some <| fun r -> (fst r.Var).Init = Undef) agentExprTr
    
    let linkTr =
        p.TrExpr (fun (v, cmp) offset -> trref p.TrLoc (p.TrLinkId cmp) v offset) p.TrLinkId
        |> p.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)
    
    let initTr (v, i) tid =
        let bexprs = Frontend.initBExprs (p.InitId tid) (v, i)
        p.TrBExpr None (p.TrExpr (trref p.TrInitLoc (string tid)) (fun () -> (string tid)))
        |> fun f -> List.map f bexprs

    let mainGuardTr =
        p.TrBExpr None (p.TrExpr (trref p.TrLoc "firstAgent") (fun () -> "firstAgent"))
    
    let propTr =
        translateProp p.TrExpr (p.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)) p.TrLoc
    
    {
        AgentExprTr = agentExprTr
        AgentGuardTr = agentGuardTr
        InitTr = initTr
        LinkTr = linkTr
        MainGuardTr = mainGuardTr
        PropTr = propTr
        TemplateInfo = p.TemplateInfo
    }