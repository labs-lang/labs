module internal Templates
open Types
open Base

let rng = System.Random()

let indent num (s:string) = 
    s.Split "\n"
    |> Seq.map (sprintf "%s%s" (String.replicate num " "))
    |> String.concat "\n"
    
let assume = sprintf "__VERIFIER_assume(%s);\n"
let inlineassertion = sprintf "assert(%s);"

let forLoop i j s = 
    (indent 4 s) |>
    (sprintf """
for (i=%i; i<%i; i++) {
%s
}
""" i j)

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")

let def name = 
    let typeofVar = function
        | a, b when a >= 0     && b < 256      -> "unsigned char "
        | a, b when a > -128   && b < 128      -> "char "
        | a, b when a >= 0     && b < 65536    -> "unsigned short "
        | a, b when a > -32768 && b < 32768    -> "short "
        | a, b when a >=0                      -> "unsigned int "
        | _ -> "int "
    function
    | Choose l -> (typeofVar (List.min l, List.max l)) + name + ";\n"
    | Range(minI, maxI) -> (typeofVar (minI, maxI)) + name + ";\n"

let assign var cVarName index =
    sprintf "%s = %s;" (translateLocation var.location "i" index) cVarName
    + (match var.location with 
        | L -> sprintf "\nLtstamp[%s][%i] = j++;" "i" index
        | _ -> "")

let initVarSim (mapping:KeyMapping) (var:Var) init =
    let baseIndex = snd mapping.[var.name]
    let initValue = 
        match init with
        | Choose(l) -> l.Item (rng.Next l.Length)
        | Range(minI, maxI) -> rng.Next(minI, maxI)
        |> string
    match var.vartype with
    | Scalar -> assign var initValue baseIndex
    | Array(s) ->
        seq [baseIndex..baseIndex+s-1]
        |> Seq.map (assign var initValue)
        |> String.concat "\n"


let initVar (mapping:KeyMapping) (var:Var) init =
    let baseIndex = snd mapping.[var.name]
    let cVarName = sprintf "guess%i%A" baseIndex var.location
    let cVarDef = def cVarName init

    let cVarAssume =
        match init with
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
        | Array(s) ->
        seq [baseIndex..(baseIndex+s-1)]
        |> Seq.map (assign var cVarName)
        |> String.concat "\n"
    )

let serializeInfo (sys, mapping:KeyMapping) =
    let serializeKeys (m:KeyMapping) =
        if m.Count = 0 then 
            ""
        else
            m
            |> Map.toSeq
            |> Seq.sortBy (fun (name,_) -> snd mapping.[name])
            |> Seq.map fst
            |> String.concat ","

    let ranges = 
        sys.spawn
        |> Map.map (fun k (cmin, cmax) -> sprintf "%s %i,%i" k cmin cmax)
        |> Map.values
        |> fun x -> if Seq.isEmpty x then "" else String.concat ";" x

    [|I;L;E|]
    |> Seq.map (fun t -> 
        mapping
        |> Map.filter (fun _ (var, _) -> var.location = t) 
        |> serializeKeys)
    |> fun x -> if Seq.isEmpty x then "" else String.concat "\n" x
    |> fun s -> printfn "%s\n%s" s ranges

    Result.Ok 0