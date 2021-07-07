module LabsTranslate.TranslationKit
open Frontend
open LabsCore
open LabsCore.Grammar
open System.IO

/// Checks that a scalar is not treated as an array and vice versa.
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

/// Translates a boolean expression.
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
            let amin, amax = table.Spawn.[agent]

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

type ITranslateConfig =
    abstract member InitId : int -> LeafExpr<'b>
    abstract member AgentName : string
    abstract member TemplateInfo : TemplateInfo
    abstract member TrLoc<'a> : Location -> string -> 'a -> string
    abstract member TrInitLoc<'a> : Location -> string -> 'a -> string
    abstract member TrLinkId : LinkComponent -> string
    abstract member TrExpr<'a, 'b> : RefTranslator<'a> -> ('b -> string) -> Expr<'a, 'b> -> string
    abstract member TrBExpr<'a, 'b when 'a:comparison and 'b:comparison> : (Ref<'a, 'b> -> bool) option -> (Expr<'a, 'b> -> string) -> BExpr<'a, 'b> -> string
        
// let makeTranslator (wrapper: ITranslateConfig) trRef trId filter =
//     let expr = wrapper.TrExpr trRef trId
//     expr, wrapper.TrBExpr filter expr


/// Creates a translation kit from the given configuration
let makeTranslationKit (conf:ITranslateConfig) =
    let agentExprTr = conf.TrExpr (trref conf.TrLoc conf.AgentName) (fun () -> conf.AgentName)
    let agentGuardTr = conf.TrBExpr (Some <| fun r -> (fst r.Var).Init = Undef) agentExprTr
    
    let linkTr =
        conf.TrExpr (fun (v, cmp) offset -> trref conf.TrLoc (conf.TrLinkId cmp) v offset) conf.TrLinkId
        |> conf.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)
    
    let initTr (v, i) tid =
        let bexprs = Frontend.initBExprs (conf.InitId tid) (v, i)
        conf.TrBExpr None (conf.TrExpr (trref conf.TrInitLoc (string tid)) (fun () -> (string tid)))
        |> fun f -> List.map f bexprs

    let mainGuardTr =
        conf.TrBExpr None (conf.TrExpr (trref conf.TrLoc "firstAgent") (fun () -> "NatToInt(Nat(tid))"))
    
    let propTr =
        translateProp conf.TrExpr (conf.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)) conf.TrLoc
    
    {
        AgentExprTr = agentExprTr
        AgentGuardTr = agentGuardTr
        InitTr = initTr
        LinkTr = linkTr
        MainGuardTr = mainGuardTr
        PropTr = propTr
        TemplateInfo = conf.TemplateInfo
    }
 
/// Provides the translation kit configuration for C.
module internal C =
    let private translateLocation = function
        | I -> sprintf "I[%s][%O]"
        | L _ -> sprintf "Lvalue[%s][%O]"
        | E -> (fun _ -> sprintf "E[%O]")

    let private translate trRef trId expr =
        let leafFn = function
            | Id i -> trId i
            | Const i -> string i
            | Extern s -> s 
        let arithmFn = function
            | Plus -> sprintf "(%s) + (%s)"
            | Minus -> sprintf "(%s) - (%s)"
            | Times -> sprintf "(%s) * (%s)"
            | Div -> sprintf "(%s) / (%s)"
            | Mod -> sprintf "mod(%s, %s)"
            | Max -> sprintf "__max(%s, %s)"
            | Min -> sprintf "__min(%s, %s)"
        let unaryFn = function
            | UnaryMinus -> sprintf "-(%s)"
            | Abs -> sprintf "__abs(%s)"
        let nondetFn = sprintf "nondetInRange(%s, %s)"
        Expr.cata leafFn arithmFn unaryFn nondetFn trRef expr

    let rec private trBExprC filter trExpr bexpr =
        let bleafFn b = if b then "1" else "0"
        let negFn = sprintf "!(%s)"
        let compareFn op e1 e2 = sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2) //TODO
        let compoundFn = function
            | Conj -> List.map (sprintf "(%s)") >> String.concat " & "
            | Disj -> List.map (sprintf "(%s)") >> String.concat " | "
        translateBExpr bleafFn negFn compareFn compoundFn filter bexpr

    let wrapper = { 
        new ITranslateConfig with
            member __.TemplateInfo = {BaseDir = "templates"; Extension = "c"}
            member __.AgentName = "tid"
            member __.InitId n = Const n
            member __.TrLinkId x = match x with | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
            member __.TrBExpr filter trExpr b = trBExprC filter trExpr b
            member __.TrExpr trRef trId e = translate trRef trId e
            member __.TrLoc loc x y = translateLocation loc x y
            member __.TrInitLoc loc x y = translateLocation loc x y
        }

/// Provides the translation kit configuration for LNT.
module internal Lnt =
    let private translateLocation loc n e =
        let name =
            match n with
            | "agent.id"
            | "NatToInt(Nat(agent.id))" -> "agent"
            | "firstAgent" -> "a"
            | "a1" | "a2" -> n
            | _ -> sprintf "agents[%s]" n 
        match loc with
        | I -> sprintf "%s.I[IntToNat(%O)]" 
        | L _ -> sprintf "%s.L[IntToNat(%O)]"
        | E -> fun _ -> sprintf "E[IntToNat(%O)]"
        |> fun f -> f name e

    let translateInitLocation _ _ _ = "x"

    let private translateExpr trRef trId =
        let leafFn = function
            | Id i -> sprintf "(%s of Int)" (trId i)
            | Const i -> sprintf "(%i of Int)" i
            | Extern s -> s (*THIS SHOULD NEVER MATCH *)
        let arithmFn = function
            | Plus -> sprintf "(%s) + (%s)"
            | Minus -> sprintf "(%s) - (%s)"
            | Times -> sprintf "(%s) * (%s)"
            | Div -> sprintf "(%s) div (%s)"
            | Mod -> sprintf "(%s) mod (%s)"
            | Max -> sprintf "max(%s, %s)"
            | Min -> sprintf "min(%s, %s)"
        let unaryFn = function
            | UnaryMinus -> sprintf "-(%s)"
            | Abs -> sprintf "abs(%s)"
        let nondetFn = fun _ _ -> failwithf "nondet expressions are currently not supported in LNT"
        Expr.cata leafFn arithmFn unaryFn nondetFn trRef

    let rec private trBExprLnt filter trExpr bexpr =
        let bleafFn b = if b then "true" else "false"
        let negFn = sprintf "(not(%s))"
        let compareFn op e1 e2 = sprintf "((%s) %O (%s))" (trExpr e1) op (trExpr e2)
        let compoundFn = function
            | Conj -> List.map (sprintf "(%s)") >> String.concat " and "
            | Disj -> List.map (sprintf "(%s)") >> String.concat " or "
            
        translateBExpr bleafFn negFn compareFn compoundFn filter bexpr
        
    let wrapper = { 
        new ITranslateConfig with
            member __.TemplateInfo = {BaseDir = "templates/lnt"; Extension = "lnt"}
            member __.AgentName = "NatToInt(Nat(agent.id))"
            member __.InitId _ = Extern "NatToInt(Nat(a.id))"
            member __.TrLinkId x = match x with | C1 -> "a1" | C2 -> "a2"
            member __.TrBExpr filter trExpr b = trBExprLnt filter trExpr b
            member __.TrExpr trRef trId e = translateExpr trRef trId e
            member __.TrLoc loc x y = translateLocation loc x y
            member __.TrInitLoc loc x y = translateInitLocation loc x y
    }
