module internal Frontend.Checks
open LabsCore.Grammar
open Message
open Outcome

/// Checks if a list of definitions contains duplicates
let duplicatesBy groupingFn defFn (lst: 'a list) =
    lst
    |> List.groupBy groupingFn
    |> List.filter (fun x -> List.length (snd x) > 1)
    |> List.map (fun (_, dupes) ->
        let defs:Node<_> list = List.map defFn dupes
        let where = List.map (fun (x:Node<_>) -> x.Pos) defs
        { What=Duplicate defs.Head.Name; Where=where} )
    |> wrap () []
    
let dupNames lst = duplicatesBy byName id lst

let checkAgent envAndLstigVars (a:Node<Agent>) table =
    let hasBehavior =
        List.exists (fun (x:Node<Process<_>>) -> x.Name = "Behavior") a.Def.Processes
        |> fun x -> if x then zero () else wrap () [] [{What=UndefBehavior a.Name; Where=[a.Pos]}]
    zero table
    // TODO check that agent only accesses stigmergy vars belonging to its stigmergies
    <??> hasBehavior
    <??> dupNames (List.append a.Def.Iface envAndLstigVars)
    <??> dupNames a.Def.Processes       