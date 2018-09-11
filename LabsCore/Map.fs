module Map
/// Returns the set of keys in m.
let keys m =
    if Map.count m = 0
    then Set.empty
    else m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
/// Returns a sequence of all the values in m.
let values table =
    if Map.count table = 0
    then Seq.empty
    else table |> Map.toSeq |> Seq.map snd

/// Builds a new map made by adding all the elements of other into table.
/// Values with the same key will be overwritten.
let merge other table =
    Map.fold (fun acc key value -> Map.add key value acc) table other
    
/// Builds a new collection whose elements are the result of
/// applying the given function to each value in the map.
/// The key is ignored.
let mapValues mapping table =
    Map.map (fun _ -> mapping) table