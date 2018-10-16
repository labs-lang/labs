module internal EncodeInit
open Types
open Base
open Templates
open Liquid

let initVar (mapping:KeyMapping) (var:Var) =
    let baseIndex = snd mapping.[var.name]
    let cVarName = sprintf "guess%i%O" baseIndex var.location
    let cVarDef = def cVarName var.init

    let cVarAssume =
        match var.init with
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
        |> Set.filter (fun v -> v.init <> Undef)
        |> Set.map (initVar mapping)
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
            (str + (forLoop rangeStart rangeEnd inits))) ""
    let makeTuples (mapping:KeyMapping) =
        /// Finds the min and max indexes of the given tuple.
        let extrema (tup:Set<Var>) =
            let indexes = 
                tup
                |> Seq.map (fun v -> snd mapping.[v.name])
            (Seq.min indexes, Seq.max indexes)

        let doLiquid (tup: Set<Var>) =
            let extr = extrema tup
            tup 
            |> Seq.map (fun v -> Dict [
                "index", Int (snd mapping.[v.name])
                "start", Int (fst extr)
                "end", Int (snd extr)
                ])

        sys.stigmergies
        |> Map.values
        |> Seq.map (fun s -> s.vars)
        |> Seq.collect (Seq.collect doLiquid)
        
    [
        "initenv", sys.environment |> initMap |> indent 4 |> Str;
        "initvars", initAll |> indent 4 |> Str;
        "initpcs", (initPc sys trees) |> indent 4 |> Str;
        "tuples", Lst (makeTuples mapping)
    ]
    |> renderFile "templates/init.c"    
