module internal Templates
open Types
open Base

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
    | I -> sprintf "I[%s][%s]"
    | L -> sprintf "Lvalue[%s][%s]"
    | E -> (fun _ -> sprintf "E[%s]")

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

//let initSimulate i keyInfo values =
//    let ar key i = arrayname key (sprintf "%i" i)
//    let ltstamp key i = initLtstamp key (sprintf "%i" i)

//    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location
//    let rng = System.Random()
//    sprintf "%s[%i] = %i;\n%s" (ar keyInfo i) keyInfo.index
//        (match values with
//        | Choose(l) -> l.Item (rng.Next l.Length)
//        | Range(minI, maxI) -> rng.Next(minI, maxI))
//        (ltstamp keyInfo i)

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
    let assign index =
        sprintf "%s = %s;" 
            (translateLocation var.location "i" (index.ToString())) 
            cVarName
        + (match var.location with 
            | L -> sprintf "\nLtstamp[%s][%i] = j++;" "i" index
            | _ -> "")

    cVarDef +
    cVarAssume + (
        match var.vartype with
        | Scalar -> assign baseIndex
        | Array(s) ->
        seq [baseIndex..(baseIndex+s-1)]
        |> Seq.map assign
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