module internal Templates
open Types
open Base

let rng = System.Random()

let indent num (s:string) = 
    s.Split "\n"
    |> Seq.map (sprintf "%s%s" (String.replicate num " "))
    |> String.concat "\n"

let assume = sprintf "LABSassume(%s);\n"
let inlineassertion = sprintf "LABSassert(%s, %s);"

let forLoop i j s = 
    (indent 4 s) |>
    (sprintf """
for (i=%i; i<%i; i++) {
%s
}
""" i j)

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let def name init = 
    let typeofVar = function
        | a, b when a >= 0     && b < 256      -> "unsigned char"
        | a, b when a > -128   && b < 128      -> "char"
        | a, b when a >= 0     && b < 65536    -> "unsigned short"
        | a, b when a > -32768 && b < 32768    -> "short"
        | a, _ when a >=0                      -> "unsigned int"
        | _ -> "int "
    match init with
        | Undef -> "int"
        | Choose l -> (typeofVar (List.min l, List.max l))
        | Range(minI, maxI) -> (typeofVar (minI, maxI))
    |> fun v -> sprintf "%s %s;\n" v name

let assign var cVarName index =
    sprintf "%s = %s;" (translateLocation var.location "i" index) cVarName
    + (match var.location with 
        | L(_) -> sprintf "\nLtstamp[%s][%i] = j++;" "i" index
        | _ -> "")

let serializeInfo (sys, mapping:KeyMapping) =
    let serializeInit = function
        | Undef -> "undef"
        | Range(min, max) -> sprintf "range %i %i" min max
        | Choose l -> l |> List.map string |> String.concat " " |> (+) "choice "

    let serializeKeys (m:seq<Var*int>) =
        m
        |> Seq.sortBy (fun (_, i) -> i)
        |> Seq.map (fun (v, _) ->
            match v.vartype with
            | Scalar -> v.name
            | Array s -> 
                seq [0..s-1] 
                |> Seq.map (sprintf "%s[%i]" v.name)
                |> String.concat ","
            |> fun s -> sprintf "%s=%s" s (serializeInit v.init))
        |> String.concat ","
        |> fun x -> if x.Length = 0 then "\n" else x

    let ranges = 
        sys.spawn
        |> Map.map (fun k (cmin, cmax) -> sprintf "%s %i,%i" k cmin cmax)
        |> Map.values
        |> fun x -> if Seq.isEmpty x then "" else String.concat ";" x

    let empty = [|0, ""; 1, ""; 2, ""|] |> Map.ofArray
    
    mapping 
    |> Map.values
    |> Seq.groupBy (fun (v,_) -> match v.location with I -> 0 | E -> 1 | _ -> 2)
    |> Seq.map ( fun (i, x) -> i, serializeKeys x)
    |> Map.ofSeq
    |> (fun x -> Map.merge x empty)
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> String.concat "\n"
    |> fun s -> printfn "%s\n%s" s ranges

    Result.Ok 0