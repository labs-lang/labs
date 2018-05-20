module Properties
open Types
open Base
open Templates

let rec encodeProp sys (mapping:Map<string*TypeofKey,int>)  (sub:Map<string, string>) = 
    let makeAssumptions c (cmin, cmax) = 
        sprintf "%s >= %i && %s < %i" c cmin c cmax
        |> assume |> (sprintf "int %s;\n%s" c)

    let encodeTerm = function
    | ConstTerm(Int(i)) -> sprintf "%i" i
    | ConstTerm(P(x,y)) -> translatePoint x y
    | KeyRef(k,c) ->
        let csub = sub.TryFind c |> Option.defaultValue c
        (translateKey mapping csub k)

    function
    | Prop(t1, t2) ->
        sprintf "%s == %s" (encodeTerm t1) (encodeTerm t2) |> assertion
    | All(compType, comp, prop) -> 
        sys.spawn.[compType]
        |> (fun (x, y) -> [x..y-1])
        |> List.map (fun i -> encodeProp sys mapping (sub.Add (comp, i.ToString())) prop)
        |> String.concat ""
    | Exists(compType, comp, prop) -> 
        makeAssumptions comp sys.spawn.[compType] +
        encodeProp sys mapping sub prop

let translateProperties sys mapping properties =
    properties
    |> Map.map (fun _ -> function Finally(p)| Always(p) -> p)
    |> Map.map (fun _ -> encodeProp sys mapping Map.empty)
    |> Map.map (fun name -> sprintf "//Property %s\n%s" name)
    |> Map.values
    |> String.concat "\n"

let translateFinallyProperties sys mapping =
    sys.properties
    |> Map.filter (fun _ -> function Finally(p) -> true | _ -> false)
    |> translateProperties sys mapping

let translateAlwaysProperties sys mapping =
    sys.properties
    |> Map.filter (fun _ -> function Always(p) -> true | _ -> false)
    |> translateProperties sys mapping