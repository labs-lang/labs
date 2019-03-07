module internal EncodeInit
open Types
open Base
open Templates
open Liquid
open Expressions
open LabsCore
open FSharpPlus.Lens

let initVar (mapping:KeyMapping) tid (var:Var) =
    let constExprTranslator = (constExpr tid).ExprTranslator
    let baseIndex = mapping.[var.name]
    let cVarName s = "_" + (translateLocation var.location tid s)

    let initAssumption i =
        let v = cVarName (string i)
        match var.init with
        | Undef -> sprintf "%s == undef_value" v |> assume
        | Choose l when l.Length = 1 ->
            sprintf "%s = (%s);\n" v (constExprTranslator l.Head)
        | Choose l ->
            l
            |> Seq.map (sprintf "(%s == (%s))" v << constExprTranslator)
            |> String.concat " | "
            |> assume
        | Range(minE, maxE) ->
            sprintf "(%s >= (%s)) & (%s < (%s))" 
                v (constExprTranslator minE) v (constExprTranslator maxE)
            |> assume
        + match var.location with 
            | L _ -> sprintf "Ltstamp[%s][tupleStart[%i]] = now();\n" tid i
            | _ -> ""

    match var.vartype with
    | Scalar -> initAssumption baseIndex
    | Array s ->
        seq [baseIndex..(baseIndex+s-1)]
        |> Seq.map initAssumption
        |> String.concat ""

    
let translateInit sys (entrypoints:EntryPoint<_>) (mapping:KeyMapping) =
    // Find the initial PC values for each agent
    let initPcs =
        sys.components
        |> Map.mapValues (fun a -> Process.entry a.processes.["Behavior"])
        |> Map.mapValues (Set.map (fun x -> entrypoints.[x]^._1))
        |> Map.map (fun name entry ->
            let minI, maxI = sys.spawn.[name]
            let entries =
                joinEntrypoints entry
                |> Map.map (fun pc values ->
                    Dict ["pc", Int pc; "values", values |> Seq.map Int |> Lst])
                |> Map.values
            Dict [
                "start", Int minI
                "end", Int maxI
                "pcs", Lst entries
            ])
        |> Map.values
        
    let initMap tid m = 
        m
        |> Set.map (initVar mapping tid)
        |> String.concat ""

    let initRange x (min, max) = 
        let initlstig name i =
            sys.stigmergies.[name].vars
            |> List.map (initMap i)
            |> String.concat "\n"
        sys.components.[x].lstig
        |> List.map (fun name -> List.map ((initlstig name) << string) [min..max-1])
        |> List.map (String.concat "")
        |> fun x -> x
        |> List.append
            (List.map (fun i -> (initMap (string i) sys.components.[x].iface)) [min..max-1])
        |> String.concat ""

    let initAll = 
        sys.spawn
        |> Map.map initRange
        |> Map.values
        |> String.concat "\n"

    [
        "initenv", sys.environment |> initMap "" |> indent 4 |> Str
        "initvars", initAll |> indent 4 |> Str
        "initpcs", initPcs |> Lst
    ]
    |> renderFile "templates/init.c"    
