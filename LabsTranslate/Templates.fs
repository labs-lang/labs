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

let translateLocation = function
    | I -> sprintf "I[%s][%O]"
    | L _ -> sprintf "Lvalue[%s][%O]"
    | E -> (fun _ -> sprintf "E[%O]")


let assign var cVarName index =
    sprintf "%s = %s;" (translateLocation var.location "i" index) cVarName
    + (match var.location with 
        | L(_) -> sprintf "\nLtstamp[%s][%i] = j++;" "i" index
        | _ -> "")

let serializeInfo (sys, mapping:KeyMapping) =
    let serializeVars (m:Set<Var>) =
        if Set.isEmpty m then "" else 
            m
            |> Set.map (fun v -> v.name, sprintf "%i=%s=%O" mapping.[v.name] v.name v.init)
            |> Seq.sortBy (fun (name, _) -> mapping.[name])
            |> Seq.map snd
            |> String.concat ";"

    let serializeSpawn k (cmin, cmax) = 
        let iface = serializeVars sys.components.[k].iface
        let lstig = 
            sys.components.[k].lstig
            |> List.collect (fun x -> sys.stigmergies.[x].vars)
            |> Set.unionMany
            |> serializeVars
        sprintf "%s %i,%i\n%s\n%s" k cmin cmax iface lstig
        
    printfn "%s" (serializeVars sys.environment)
    sys.spawn
    |> Map.map serializeSpawn
    |> Map.iter (fun _ -> printfn "%s")
    |> ignore

    Result.Ok 0