module List

/// Returns a list of duplicates according to function fn.
let duplicatesBy fn lst = 
    lst
    |> List.groupBy fn
    |> List.filter (fun (_, y) -> y.Length > 1)
    |> List.collect snd

/// Returns a list of elements that appear more than once in lst.
let duplicates lst = duplicatesBy id lst

///Cartesian product of a variable number of lists.
/// Input is a list of lists of which the cartesian product is to be constructed;
/// output is a list that contains the elements of the product set, as lists.
///https://www.fssnip.net/2A/title/Cartesian-product-of-n-lists
/// // Takes an input like [[1;2;5];[3;4];[6;7]] and returns
// [[5; 3; 7]; [2; 3; 7]; [1; 3; 7]; [5; 4; 7]; [2; 4; 7];
// [1; 4; 7]; [5; 3; 6]; [2; 3; 6]; [1; 3; 6]; [5; 4; 6];
// [2; 4; 6]; [1; 4; 6]]
let rec cartesian lstlst =
    match lstlst with
    | [ h ] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesian t)
    | _ -> []
