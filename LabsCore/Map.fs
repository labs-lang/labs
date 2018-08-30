module Map
/// Returns the set of keys in m.
let keys table =
    if Map.count table = 0
    then Set.empty
    else table |> Map.toSeq |> Seq.map fst |> Set.ofSeq
/// Returns a sequence of all the values in m.
let values table =
    if Map.count table = 0
    then Seq.empty
    else table |> Map.toSeq |> Seq.map snd

/// Builds a new map made by adding all the elements of other into table.
/// Values with the same key will be overwritten.
let merge other table =
    Map.fold (fun acc key value -> Map.add key value acc) table other

/// Builds a new map by applying the given function to each element of the table.
/// The first argument of the function is the index of the element being transformed,
/// starting from start.
let mapiFrom start mapping table  = 
    table
    |> Map.fold (
        fun (index, result) k v -> 
            (index+1, (Map.add k (mapping index k v) result))) (start, Map.empty) 
    |> snd

/// Builds a new map by applying the given function to each element of the table.
/// The first argument of the function is the index of the element being transformed,
/// starting from 0.
let mapi mapping table =
    mapiFrom 0 mapping table