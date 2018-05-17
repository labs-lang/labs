module List
    /// Returns a list of elements that appear more than once in lst.
let duplicates lst = 
    lst
    |> List.groupBy id
    |> List.filter (fun (x, y) -> y.Length > 1)
    |> List.map fst