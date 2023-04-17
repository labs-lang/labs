module LabsTranslate.TranslationKit
open Frontend
open Frontend.Message
open LabsCore
open LabsCore.ExprTypes
open LabsCore.Expr
open LabsCore.BExpr
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
    getVars expr
    |> Set.filter (fun (v, _) -> isLstigVar v)

/// Translates a variable reference.
let private trref trLocation name (v:Var<int>, i:int) offset ofAgent =
    do refTypeCheck v offset //TODO move
    let agent = match ofAgent with None -> name | Some a -> a
    let index =
        match offset with
        | None -> string i
        | Some indexes ->
            let dims = match v.Vartype with Array s -> s | _ -> []
            let offsets =   
                [0..dims.Length-1]
                |> List.map (fun i -> List.reduce (*) (1::List.rev(dims)).[..i])
                |> List.rev |> List.map string
            if offsets.Length <> List.length indexes then
                failwith $"Cannot zip {offsets} and {indexes} (in trref)"
            List.zip offsets indexes
            |> List.map (fun (off, i) -> $"({off} * {i})")
            |> String.concat " + "
            |> fun linearOffset -> $"%i{i} + {linearOffset}"
    match v.Location with
    | Local -> v.Name
    | Pick _ ->
        let off =
            match offset with
            | None -> ""
            | Some [off] -> $"[{off}]"
            | Some o -> failwith $"Illegal pick offset '{o}'" 
        $"""{v.Name.Replace("[]", "")}{off}"""  
    | _ -> trLocation v.Location agent index

/// Translates a boolean expression.
let translateBExpr bleafFn negFn compareFn compoundFn filter bexpr =
    let undefs =
        match filter with
        | None -> Set.empty
        | Some f ->
            /// Checks that expr is "well-defined"
            /// (i.e., referenced vars must not be equal to "undef_value")
            let checkWellDef expr =
                Set.filter f (getRefs expr)
                |> Set.map (fun r -> Compare(Ref(r), Neq, Leaf(Extern "undef_value")))
            cata (fun _ -> Set.empty) id (fun _ e1 e2 -> Set.union (checkWellDef e1) (checkWellDef e2)) (fun _ -> Set.unionMany) bexpr
        
    if undefs.IsEmpty then bexpr
    else
        Set.add bexpr undefs
        |> fun s -> Compound(Conj, s |> Set.toList)
    |> cata bleafFn negFn compareFn compoundFn
    
let private translateQPred trExpr trBExpr trLocation name (table:SymbolTable) qp =
    let ex = Map.exists (fun _ (_, q)-> q = Exists) qp.Quantifiers
    let fa = Map.exists (fun _ (_, q)-> q = All) qp.Quantifiers
    
    //TODO move checks to frontend
    let translateSub (sub:Map<_,_>) =
        let propId name =
            match sub.TryFind name with
            | Some e -> e
            | None -> failwithf $"Undefined agent {name}"
        
        let propRef1 ((v:Var<_>, i), c) ref_ofAgent offset ofagent =
            match c, ref_ofAgent with
            | None, None ->
                // no "of"
                if v.Location <> E then failwithf $"{v.Name} is not an environment variable"
                {Var=((v, i), c); Offset=offset; OfAgent=None}
            | None, _ ->
                // "x of (numeric constant)"
                {Var=((v, i), c); Offset=offset; OfAgent=ref_ofAgent}
            | Some c, _ ->
                // "x of (variable)"
                {Var=((v, i), (Some <| (string << propId) c)); Offset=offset; OfAgent=ofagent}

        let trLeaf leaf =
            match leaf with
            | Id name -> (string << propId) name |> Id
            | _ -> leaf
        
        map BLeaf (Expr.map trLeaf (fun r -> propRef1 r.Var r.OfAgent))
        
    if (ex && fa) then
        if name <> "" then $"Property {name}: " else ""
        |> fun n -> failwith $"{n}alternating quantifiers are currently not supported"

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
            |> simplify
        else
            (translateSub subs) prop.Predicate
    
    let propRef ((v:Var<_>, i), c) offset =
        match c with
        | None -> trref trLocation "" (v, i) offset
        | Some c -> (trref trLocation c (v, i) offset)

    let rec trb bexpr = trBExpr tre bexpr 
    and tre = trExpr propRef id trb
    
    trProp Map.empty qp |> trb

let private translateProp trExpr trBExpr trLocation (table:SymbolTable) (p:Node<Property<_>>) =
    translateQPred trExpr trBExpr trLocation p.Name table p.Def.QuantPredicate

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
    QPredTr: SymbolTable -> QuantPredicate<Var<int> * int> -> string
    TemplateInfo : TemplateInfo
    CollectAuxVars : Expr<Var<int> * int, unit> -> Set<string * string * string>
}

type RefTranslator<'a> = 'a -> string list option -> string option -> string

type ITranslateConfig =
    abstract member InitId : int -> LeafExpr<'b>
    abstract member AgentName : string
    abstract member TemplateInfo : TemplateInfo
    abstract member TrLoc<'a> : Location -> string -> 'a -> string
    abstract member TrInitLoc<'a> : Location -> string -> 'a -> string
    abstract member TrLinkId : LinkComponent -> string
    abstract member TrExpr<'a, 'b> : RefTranslator<'a> -> ('b -> string) -> (BExpr<'a, 'b> -> string) -> Expr<'a, 'b> -> string
    abstract member TrBExpr<'a, 'b when 'a:comparison and 'b:comparison> : (Ref<'a, 'b> -> bool) option -> (Expr<'a, 'b> -> string) -> BExpr<'a, 'b> -> string
    abstract member CollectAuxVars : (Expr<'a, 'b> -> string) -> Expr<'a, 'b> -> Set<string * string * string>


/// Creates a translation kit from the given configuration
let makeTranslationKit (conf:ITranslateConfig) =
    
    let guardTr exprTranslate bexpr = conf.TrBExpr None exprTranslate bexpr 
    
    let rec mainGuardTr bexpr =
        let tr = conf.TrExpr (trref conf.TrLoc "firstAgent") (fun () -> conf.AgentName) mainGuardTr
        guardTr tr bexpr
    
    // TODO check that ids are translated correctly
    let rec agentExprTr expr =
        conf.TrExpr (trref conf.TrLoc conf.AgentName)  (fun () -> conf.AgentName) (guardTr agentExprTr) expr
    let agentGuardTr = conf.TrBExpr (Some <| fun r -> (fst r.Var).Init = Undef) agentExprTr
    
    let rec linkTr bexpr =
        let trLinkExpr = conf.TrExpr (fun (v, cmp) -> trref conf.TrLoc (conf.TrLinkId cmp) v) conf.TrLinkId linkTr
        conf.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef) trLinkExpr bexpr
    
    let initTr (v, i) tid =
        let bexprs = Frontend.initBExprs (conf.InitId tid) (v, i)
        let rec trBExpr b =
            conf.TrBExpr
                None
                (conf.TrExpr (trref conf.TrInitLoc (string tid)) (fun () -> (string tid)) trBExpr)
                b
        List.map trBExpr bexprs

    
    let propTr =
        translateProp conf.TrExpr (conf.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)) conf.TrLoc
    let qpredTr =
        translateQPred conf.TrExpr (conf.TrBExpr (Some <| fun r -> ((fst << fst) r.Var).Init = Undef)) conf.TrLoc ""
    
    {
        AgentExprTr = agentExprTr
        AgentGuardTr = agentGuardTr
        InitTr = initTr
        LinkTr = linkTr
        MainGuardTr = mainGuardTr
        QPredTr = qpredTr
        PropTr = propTr
        TemplateInfo = conf.TemplateInfo
        CollectAuxVars = conf.CollectAuxVars agentExprTr
    }
 
/// Provides the translation kit configuration for C.
module internal C =
    let private translateLocation = function
        | I -> sprintf "I[%s][%O]"
        | L _ -> sprintf "Lvalue[%s][%O]"
        | E -> (fun _ -> sprintf "E[%O]")
        | Local | Pick _ -> fun _ _ -> "" 

    let private translate trRef trId trBExpr expr =
        let leafFn = function
            | Id i -> trId i
            | Const i -> string i
            | Extern s -> s 
        let arithmFn = function
            | Plus -> sprintf "(%s) + (%s)"
            | Minus -> sprintf "(%s) - (%s)"
            | Times -> sprintf "(%s) * (%s)"
            | Div -> sprintf "(%s) / (%s)"
            | RoundDiv -> sprintf "__round_div(%s, %s)"
            | Mod -> sprintf "mod(%s, %s)"
            | Max -> sprintf "__max(%s, %s)"
            | Min -> sprintf "__min(%s, %s)"
        let unaryFn = function
            | UnaryMinus -> sprintf "-(%s)"
            | Abs -> sprintf "__abs(%s)"
        let nondetFn e1 e2 _ = sprintf $"nondetInRange({e1}, {e2})"
        let rawFn name args = $"""{name}({String.concat ", " args})"""
        let ifFn cond ift iff =
            if ift = "1" && iff = "0"
            then $"({trBExpr cond})"
            else $"(%s{trBExpr cond}) ? ({ift}) : ({iff})"
        
        Expr.cata leafFn arithmFn unaryFn nondetFn trRef rawFn ifFn expr

    let rec private trBExprC filter trExpr bexpr =
        let bleafFn b = if b then "1" else "0"
        let negFn = sprintf "!(%s)"
        let compareFn op e1 e2 = $"((%s{trExpr e1}) {op} (%s{trExpr e2}))" //TODO
        let compoundFn = function
            | Conj -> List.map (sprintf "(%s)") >> String.concat " & "
            | Disj -> List.map (sprintf "(%s)") >> String.concat " | "
        translateBExpr bleafFn negFn compareFn compoundFn filter bexpr

    let wrapper = { 
        new ITranslateConfig with
            member _.TemplateInfo = {BaseDir = "templates/c"; Extension = "c"}
            member _.AgentName = "tid"
            member _.InitId n = Const n
            member _.TrLinkId x = match x with | C1 -> "__LABS_link1" | C2 -> "__LABS_link2"
            member _.TrBExpr filter trExpr b = trBExprC filter trExpr b
            member _.TrExpr trRef trId trBExpr e = translate trRef trId trBExpr e
            member _.TrLoc loc x y = translateLocation loc x y
            member _.TrInitLoc loc x y = translateLocation loc x y
            member _.CollectAuxVars _ _ = Set.empty
        }

/// Provides the translation kit configuration for LNT.
module internal Lnt =
    let private translateLocation loc n e =
        let name =
            match n with
            | "agent.id"
            | "NatToInt(Nat(agent.id))" -> "agent"
            | "firstAgent" -> "agent"
            | "a1" | "a2" -> n
            | _ -> $"agents[%s{n}]" 
        match loc with
        | I -> sprintf "%s.I[IntToNat(%O)]" 
        | L _ -> sprintf "%s.L[IntToNat(%O)]"
        | E -> fun _ -> sprintf "E[IntToNat(%O)]"
        | Local | Pick _  -> fun _ _ -> n
        |> fun f -> f name e

    let private translateLocationParallel loc n e =
        let forLink =
            match n with
            | "a1" | "NatToInt(Nat(id1))" -> "1"
            | "a2" | "NatToInt(Nat(id2))" -> "2"
            | _ -> ""
        match loc with
        | I -> $"I{forLink}[IntToNat({e})]"
        | L _ -> $"L{forLink}[IntToNat({e})]"
        | E -> $"E[IntToNat({e})]"
        | Local | Pick _  -> n
    
    let translateInitLocation _ _ _ = "x"

    let private translateExpr trRef trId trBExpr expr =
        let leafFn = function
            | Id i -> $"(%s{trId i} of Int)"
            | Const i -> $"(%i{i} of Int)"
            | Extern s -> s (*THIS SHOULD NEVER MATCH *)
        let arithmFn = function
            | Plus -> sprintf "(%s + %s)"
            | Minus -> sprintf "(%s - %s)"
            | Times -> sprintf "(%s * %s)"
            | Div -> sprintf "(%s div %s)"
            | RoundDiv -> sprintf "rounddiv(%s, %s)"
            | Mod -> sprintf "(%s mod %s)"
            | Max -> sprintf "max(%s, %s)"
            | Min -> sprintf "min(%s, %s)"
        let unaryFn = function
            | UnaryMinus -> sprintf "-(%s)"
            | Abs -> sprintf "abs(%s)"
        let nondetFn = fun _ _ (pos:Position) -> $"nondet_{pos.GetHashCode()}" 
        //failwith "nondet expressions are currently not supported in LNT"
        let rawFn name args = $"""{name}({String.concat ", " args})"""
        let ifFn cond ift iff = $"ifelse({trBExpr cond}, {ift}, {iff})"
        Expr.cata leafFn arithmFn unaryFn nondetFn trRef rawFn ifFn expr

    let rec private trBExprLnt filter trExpr bexpr =
        let bleafFn b = if b then "true" else "false"
        let negFn = sprintf "(not(%s))"
        let compareFn op e1 e2 = $"((%s{trExpr e1}) {op} (%s{trExpr e2}))"
        let compoundFn = function
            | Conj -> List.map (sprintf "(%s)") >> String.concat " and "
            | Disj -> List.map (sprintf "(%s)") >> String.concat " or "
            
        translateBExpr bleafFn negFn compareFn compoundFn filter bexpr
    
    let rec collectAux trExpr expr =
        let recurse = collectAux trExpr
        match expr with
        | QB _ | Count _ -> Set.empty
        | Nondet(e1, e2, pos) ->
            recurse e1 |> Set.union (recurse e2) |> Set.add ($"nondet_{pos.Line}_{pos.Column}", trExpr e1, trExpr e2)
        | IfElse (_, e1, e2) // TODO collect auxs in condition too
        | Arithm (e1, _, e2) -> recurse e1 |> Set.union (recurse e2)
        | Unary(_, e) -> recurse e
        | Ref r -> r.Offset |> Option.map (Set.unionMany << List.map recurse) |> Option.defaultValue Set.empty
        | Leaf _ -> Set.empty
        | RawCall (_, args) -> Seq.map recurse args |> Set.unionMany
    
    let wrapper = { 
        new ITranslateConfig with
            member _.TemplateInfo = {BaseDir = "templates/lnt"; Extension = "lnt"}
            member _.AgentName = "NatToInt(Nat(agent.id))"
            member _.InitId _ = Extern "NatToInt(Nat(agent.id))"
            member _.TrLinkId x = match x with | C1 -> "a1" | C2 -> "a2"
            member _.TrBExpr filter trExpr b = trBExprLnt filter trExpr b
            member _.TrExpr trRef trId trBExpr e = translateExpr trRef trId trBExpr e
            member _.TrLoc loc x y = translateLocation loc x y
            member _.TrInitLoc loc x y = translateInitLocation loc x y
            member _.CollectAuxVars tr e = collectAux tr e
    }
    let wrapperMonitor = {
        new ITranslateConfig with
            member _.TemplateInfo = {BaseDir = "templates/lnt-monitor"; Extension = "lnt"}
            member _.AgentName = "NatToInt(Nat(agent.id))"
            member _.InitId _ = Extern "NatToInt(Nat(agent.id))"
            member _.TrLinkId x = match x with | C1 -> "a1" | C2 -> "a2"
            member _.TrBExpr filter trExpr b = trBExprLnt filter trExpr b
            member _.TrExpr trRef trId trBExpr e = translateExpr trRef trId trBExpr e
            member _.TrLoc loc x y = translateLocation loc x y
            member _.TrInitLoc loc x y = translateInitLocation loc x y
            member _.CollectAuxVars tr e = collectAux tr e
    }
    
    let wrapperParallel = {
            new ITranslateConfig with
                member _.TemplateInfo = {BaseDir = "templates/lnt-parallel"; Extension = "lnt"}
                member _.AgentName = "NatToInt(Nat(id))"
                member _.InitId _ = Extern "NatToInt(Nat(id))"
                member _.TrLinkId x = match x with | C1 -> "NatToInt(Nat(id1))" | C2 -> "NatToInt(Nat(id2))"
                member _.TrBExpr filter trExpr b = trBExprLnt filter trExpr b
                member _.TrExpr trRef trId trBExpr e = translateExpr trRef trId trBExpr e
                member _.TrLoc loc x y = translateLocationParallel loc x y
                member _.TrInitLoc loc x y = translateInitLocation loc x y
                member _.CollectAuxVars tr e = collectAux tr e
        }
