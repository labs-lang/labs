module internal Properties
open Types
open Templates
open Expressions

let translateProp sys (p:Property<Var*int>) =

    let trId (sub:Map<string, int>) name = 
        string sub.[name]

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

    let ex = Map.exists (fun _ (_, q)-> q = Exists) p.quantifiers
    let fa = Map.exists (fun _ (_, q)-> q = All) p.quantifiers

    if (ex && fa) then 
        p.name
        |> sprintf "Property %s: alternating quantifiers are currently not supported"
        |> failwith

    let translateSub sub =
        (predExpr sub).BExprTranslator ("Property " + p.name)

    let rec trProp subs prop =
        if not prop.quantifiers.IsEmpty then
            let nextId = Map.pick (fun k _ -> Some k) prop.quantifiers
            let agent, quantifier = prop.quantifiers.[nextId]
            let amin, amax = sys.spawn.[agent]

            [amin..amax-1]
            |> List.map (fun i -> Map.add nextId i subs)
            |> List.map (fun s -> trProp s {prop with quantifiers=prop.quantifiers.Remove nextId})
            |> String.concat(
                match quantifier with 
                | All -> " & "
                | Exists -> " | ")
        else
            translateSub subs prop.predicate

    trProp Map.empty p
    |> fun pstring -> inlineassertion pstring p.name
    |> fun x -> sprintf "%s //%s\n" x p.name