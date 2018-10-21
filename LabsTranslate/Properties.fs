module internal Properties
open Types
open Templates
open Expressions

/// Cartesian product of a list of lists.
//http://techneilogy.blogspot.com/2010/07/cartesian-product-of-lists-in-f.html
let rec cart nll = 
    let f0 n = function
    | [] -> [[n]]
    | nll -> List.map (fun nl->n::nl) nll
    match nll with
    | [] -> []
    | h::t -> List.collect (fun n->f0 n (cart t)) h
    
let translateProp sys (p:Property<Var*int>) =

    let trId (sub:Map<string, int>) name = 
        match snd p.quantifiers.[name] with 
        | All -> name+p.name
        | Exists -> (sprintf "%i" sub.[name])

    //Given a substitution table, resolves references to quantified
    //component names.
    let tr (sub:Map<string, int>) ((v, i), c) offset =
        match c with
        | None -> 
            if v.location <> E then
                v.name
                |> sprintf "Property %s: %s is not an environment variable" p.name
                |> failwith 
            trref "" (v, i) offset
        | Some c ->
            (trref (trId sub c) (v, i) offset)

    let predExpr sub = {
        refTranslator = tr sub
        idTranslator = trId sub
        filterUndef = fun ((v, _), _) -> v.init = Undef
    }

    let exists, forall = 
        Map.partition
            (fun _ (_, q) -> match q with Exists -> true | _ -> false)
            p.quantifiers
    if not (exists.IsEmpty || forall.IsEmpty) then 
        p.name
        |> sprintf "Property %s: alternating quantifiers are currently not supported"
        |> failwith

    let subs =
        exists
        |> Map.mapValues (fun (cmpType, _) -> sys.spawn.[cmpType])
        |> Map.toList
        |> List.map (fun (id, (starts, ends)) ->
            [starts..ends-1] |> List.map (fun i -> id,i))
        |> cart
        |> List.map Map.ofList
        |> fun x -> if x.IsEmpty then [Map.empty] else x
        
    let translateSub sub =
        (predExpr sub).BExprTranslator ("Property " + p.name)

    let assumptions =
        forall
        |> Map.map (
            fun id (cmpType, _) ->
                let varName = id + p.name
                let cmin, cmax = sys.spawn.[cmpType]
                sprintf "%s >= %i && %s < %i" varName cmin varName cmax
                |> assume 
                |> (sprintf "int %s;\n%s" varName)
            )
        |> Map.values
        |> String.concat ""
    subs
    |> List.map (fun s -> translateSub s p.predicate)
    |> String.concat " || "
    |> fun pstring -> inlineassertion pstring p.name
    |> fun x -> sprintf "%s //%s\n" x p.name
    |> (+) assumptions