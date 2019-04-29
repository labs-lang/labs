module internal Checker.Checks
open LabsCore
open Message
open Outcome

/// Checks if a list of definitions contains duplicates
let duplicatesBy groupingFn defFn (lst: 'a list) =
    lst
    |> List.groupBy groupingFn
    |> List.filter (fun x -> List.length (snd x) > 1)
    |> List.map (fun (name, dupes) ->
        let defs:Node<_> list = List.map defFn dupes
        let where = List.map (fun (x:Node<_>) -> x.pos) defs
        { what=Duplicate (defs.Head.name); where=where} )
    |> wrap () []
    
let dupNames lst = duplicatesBy (fun (x:Node<_>) -> x.name) id lst

let checkAgent envAndLstigVars (a:Node<Agent>) table =
    let hasBehavior =
        List.exists (fun (x:Node<Process<_,_>>) -> x.name = "Behavior") a.def.processes
        |> fun x -> if x then zero () else wrap () [] [{what=UndefBehavior a.name; where=[a.pos]}]
    zero table
    // TODO check that agent only accesses stigmergy vars belonging to its stigmergies
    <??> hasBehavior
    <??> dupNames (List.append a.def.iface envAndLstigVars)
    <??> dupNames a.def.processes       