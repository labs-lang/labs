module internal Templates
open Types
open Base

let indent num (s:string) = 
    s.Split "\n"
    |> Seq.map (sprintf "%s%s" (String.replicate num " "))
    |> String.concat "\n"

let translateKey (mapping:KeyMapping) index k = 
    let getTranslation index = function
    | I -> sprintf "I[%s][%i]" index mapping.[k].index
    | L -> sprintf "Lvalue[%s][%i]" index mapping.[k].index
    | E -> sprintf "E[%i]" mapping.[k].index

    let tryKeyInfo = mapping.TryFind k
    match tryKeyInfo with
    | Some(info) -> getTranslation index info.location
    | None -> failwith ("Unexpected key " + k)

let updateKq keys = 
    (keys
    |> Seq.map (fun x -> sprintf "setHin(tid, %i);" x)
    |> String.concat "\n") + "\n"

let assume = sprintf "__VERIFIER_assume(%s);\n"
let assertion = sprintf "assert(%s);\n"
let inlineassertion = sprintf "assert(%s);"

let entrypoint entry =
    assume <| (sprintf "pc[tid][%i] == %i" entry.pc entry.value)

let exitpoint exit =
    sprintf "pc[tid][%i] = %i;\n" exit.pc exit.value

let forLoop i j s = 
    (indent 4 s) |>
    (sprintf """
for (i=%i; i<%i; i++) {
%s
}
""" i j)

let arrayname keyInfo i =
        match keyInfo.location with
        | I -> sprintf "I[%s]" i
        | L -> sprintf "Lvalue[%s]" i
        | E -> "E"
let initLtstamp keyInfo i = 
    match keyInfo.location with 
    | L -> sprintf "Ltstamp[%s][%i] = j++;" i keyInfo.index
    | _ -> ""

let def name = 
    let typeofVar =
        function
        | (a,b) when a >= 0     && b < 256      -> "unsigned char "
        | (a,b) when a > -128   && b < 128      -> "char "
        | (a,b) when a >= 0     && b < 65536    -> "unsigned short "
        | (a,b) when a > -32768 && b < 32768    -> "short "
        | (a,b) when a >=0                      -> "unsigned int "
        | _ -> "int "

    function
    | ChooseI(l) -> (typeofVar (List.min l, List.max l) ) + name + ";"
    | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + name + ";")

let initSimulate i keyInfo values =
    let ar key i = arrayname key (sprintf "%i" i)
    let ltstamp key i = initLtstamp key (sprintf "%i" i)

    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location
    let rng = System.Random()
    sprintf "%s[%i] = %i;\n%s" (ar keyInfo i) keyInfo.index
        (match values with
        | ChooseI(l) -> l.Item (rng.Next l.Length)
        | RangeI(minI, maxI) -> rng.Next(minI, maxI))
        (ltstamp keyInfo i)

let init keyInfo values =
    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location

    let assumeInt = sprintf "(%s == %i)" guess
    let assumeIntRange key minI maxI =
        sprintf "%s >= %i && %s < %i" guess minI guess maxI |> assume

    let assign values =
        sprintf "%s[%i] = %s;\n%s" (arrayname keyInfo "i") keyInfo.index guess (initLtstamp keyInfo "i")

    (def guess values) + "\n" + (
        match values with
        | ChooseI(l) when l.Length = 1 -> 
            sprintf "%s = %i;\n" guess l.Head
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " || "
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange keyInfo.index minI maxI)
        + (assign values)

let serializeInfo (sys, mapping:KeyMapping) =
    let serializeKeys (m:KeyMapping) =
        if m.Count = 0 then ""
        else
        m
        |> Map.toSeq
        |> Seq.sortBy (fun (_,info) -> info.index)
        |> Seq.map (fun (name, _) -> name)
        |> String.concat ","

    let maxTupleLength =
        sys.components
        |> Map.map (fun _ cdef -> cdef.lstig |> List.map (fun m -> m.Count))
        |> Map.values
        |> fun x -> if Seq.isEmpty x then Seq.empty else (Seq.concat x)
        |> fun x -> if Seq.isEmpty x then 0 else Seq.max x

    let ranges = 
        sys.spawn
        |> Map.map (fun k (cmin, cmax) -> sprintf "%s %i,%i" k cmin cmax)
        |> Map.values
        |> fun x -> if Seq.isEmpty x then "" else String.concat ";" x

    [|I;L;E|]
    |> Seq.map (fun t -> 
        Map.filter (fun _ info -> info.location = t) mapping
        |> serializeKeys)
    |> fun x -> if Seq.isEmpty x then "" else String.concat "\n" x
    |> fun s -> printfn "%s\n%s\nunwind %i" s ranges (maxTupleLength + 1)

    Result.Ok(0)