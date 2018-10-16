module List

/// Returns a list of duplicates according to function fn.
let duplicatesBy fn lst = 
    lst
    |> List.groupBy fn
    |> List.filter (fun (_, y) -> y.Length > 1)
    |> List.collect snd

/// Returns a list of elements that appear more than once in lst.
let duplicates lst = duplicatesBy id lst