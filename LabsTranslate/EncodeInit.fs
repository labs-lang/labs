module internal EncodeInit
open Types
open Base
open Templates
open Liquid

let initVar (mapping:KeyMapping) (var:Var) init =
    let baseIndex = snd mapping.[var.name]
    let cVarName = sprintf "guess%i%O" baseIndex var.location
    let cVarDef = def cVarName init

    let cVarAssume =
        match init with
        | Undef -> ""
        | Choose l when l.Length = 1 -> 
            sprintf "%s = %i;\n" cVarName l.Head
        | Choose l ->
            l
            |> Seq.map (sprintf "(%s == %i)" cVarName)
            |> String.concat " || "
            |> assume
        | Range(minI, maxI) -> //assumeIntRange index minI maxI
            sprintf "%s >= %i && %s < %i" cVarName minI cVarName maxI |> assume

    cVarDef +
    cVarAssume + (
        match var.vartype with
        | Scalar -> assign var cVarName baseIndex
        | Array s ->
            seq [baseIndex..(baseIndex+s-1)]
            |> Seq.map (assign var cVarName)
            |> String.concat "\n")

/// Renders the init() section using the given initialization function.
let translateInit (sys, trees, mapping) =
    let initPc sys trees =
        trees
        |> Map.map (fun n (_, entry) -> 
            let minI, maxI = sys.spawn.[n]
            sprintf "pc[i][0] = %i;" entry
            |> forLoop minI maxI)
        |> Map.values
        |> String.concat "\n"

    let initMap m = 
        m
        |> Map.filter (fun _ init -> init <> Undef)
        |> Map.map (initVar mapping)
        |> Map.values
        |> String.concat "\n"

    let initAll =
        sys.spawn
        |> Map.map (fun x range -> 
            let ifaceinit = sys.components.[x].iface |> initMap
            let lstigsinit = 
                sys.stigmergies
                |> Map.mapValues (fun s -> Seq.map initMap s.vars)
                |> Map.values
                |> Seq.concat
                |> String.concat "\n"
            (range, ifaceinit + "\n" + lstigsinit))
        |> Map.fold (fun str _ ((rangeStart, rangeEnd), inits) -> 
            (str + (forLoop rangeStart rangeEnd inits))) "" //FIXME
    let makeTuples comps (mapping:KeyMapping) =
        /// Finds the min and max indexes of the given tuple.
        let extrema (tup:Map<Var,Init>) =
            let indexes = 
                tup
                |> Map.toSeq
                |> Seq.map (fun (v, _) -> snd mapping.[v.name])
            (Seq.min indexes, Seq.max indexes)

        let doLiquid (tup: Map<Var,Init>) =
            let extr = extrema tup
            tup 
            |> Map.map (fun v _ -> Dict [
                "index", Int (snd mapping.[v.name])
                "start", Int (fst extr)
                "end", Int (snd extr)
                ])
            |> Map.values

        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.collect (Seq.collect doLiquid)
        
    [
        "initenv", sys.environment |> initMap |> indent 4 |> Str;
        "initvars", initAll |> indent 4 |> Str;
        "initpcs", (initPc sys trees) |> indent 4 |> Str;
        "tuples", Lst (makeTuples (Map.values sys.components) mapping)
    ]
    |> renderFile "templates/init.c"    
