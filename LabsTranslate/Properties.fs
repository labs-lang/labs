module Properties
open Types
open Base
open Templates
open Expressions

let rec Negate = 
    function
    | Prop(a, op, b) -> match op with
                        | Equal -> Prop(a, Neq, b)
                        | Neq -> Prop(a, Equal, b)
                        | Greater -> Prop(a, Leq, b)
                        | Leq -> Prop(a, Greater, b)
                        | Less -> Prop(a, Geq, b)
                        | Geq -> Prop(a, Less, b)
    | All(c, n, p) -> Exists(c, n, Negate(p))
    | Exists(c, n, p) -> All(c,n, Negate(p))


let rec encodeProp name sys (mapping:KeyMapping) (sub:Map<string, string>) = 
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
    | All(compType, comp, prop) -> 
        sys.spawn.[compType]
        |> fun (x, y) -> [x..y-1]
        |> List.map (fun i -> encodeProp name sys mapping (sub.Add (comp, i.ToString())) prop)
        |> String.concat " && "
        |> sprintf "(%s)"
    | Exists(compType, comp, prop) -> 
        sys.spawn.[compType]
        |> fun (x, y) -> [x..y-1]
        |> List.map (fun i -> encodeProp name sys mapping (sub.Add (comp, i.ToString())) prop)
        |> String.concat " || "
        |> sprintf "(%s)"
    //enc p |> fun s -> sprintf "%s //%s\n" (inlineassertion s) name

let translateProperties sys mapping properties =
    properties
    |> Map.map (fun _ -> function Finally(p)| Always(p) -> p)
    |> Map.map (fun name -> encodeProp name sys mapping Map.empty)
    |> Map.map (fun _ -> inlineassertion)
    |> Map.map (fun n assertion -> sprintf "%s //%s\n" assertion n)
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