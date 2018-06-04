module Properties
open Types
open Base
open Templates
open Expressions

let rec encodeProp name sys (mapping:KeyMapping)  (sub:Map<string, string>) = 
    let makeAssumptions c (cmin, cmax) = 
        sprintf "%s%s >= %i && %s%s < %i" c name cmin c name cmax
        |> assume |> (sprintf "int %s%s;\n%s" c name)

    let encodeTerm = function
    | ConstTerm(i) -> sprintf "%i" i
    | KeyRef(k,c) ->
        let csub = sub.TryFind c |> Option.defaultValue (c+name)
        (translateKey mapping csub k)

    function
    | Prop(t1, op, t2) ->
        sprintf "%s %s %s" (encodeTerm t1) (translateBOp op) (encodeTerm t2)
        |> fun s -> sprintf "%s //%s\n" (inlineassertion s) name
    | All(compType, comp, prop) -> 
        sys.spawn.[compType]
        |> fun (x, y) -> [x..y-1]
        |> List.map (fun i -> encodeProp name sys mapping (sub.Add (comp, i.ToString())) prop)
        |> String.concat ""
    | Exists(compType, comp, prop) -> 
        makeAssumptions comp sys.spawn.[compType] +
        encodeProp name sys mapping sub prop

let translateProperties sys mapping properties =
    properties
    |> Map.map (fun _ -> function Finally(p)| Always(p) -> p)
    |> Map.map (fun name -> encodeProp name sys mapping Map.empty)
    |> Map.values
    |> String.concat "\n"

let translateFinallyProperties sys mapping =
    sys.properties
    |> Map.filter (fun _ -> function Finally(_) -> true | _ -> false)
    |> translateProperties sys mapping

let translateAlwaysProperties sys mapping =
    sys.properties
    |> Map.filter (fun _ -> function Always(_) -> true | _ -> false)
    |> translateProperties sys mapping