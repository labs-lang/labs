module Map
/// Returns the set of keys in m.
let keys m =
    m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
/// Returns a sequence of all the values in m.
let values m =
    m |> Map.toSeq |> Seq.map snd

let merge m2 m =
    Map.fold (fun acc key value -> Map.add key value acc) m m2