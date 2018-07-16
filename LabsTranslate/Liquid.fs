module internal Liquid

open Base
open DotLiquid

let rec hash (l: ((string * obj) seq)) =
    l
    |> Seq.map (fun (k,v) -> 
        if v :? (string * obj) seq then 
            k, box (hash (unbox v))
        elif v :? (string * obj) seq seq then
            k, box (unbox v |> Seq.map hash)
        else k, v)
    |> dict
    |> Hash.FromDictionary

///<summmary>Opens a template file and renders it using the specified
///local variables.</summary>
let renderFile path (data:(string * obj) list) = 
    readFile(path)
    |> Result.map Template.Parse
    |> Result.map (fun x -> x.Render (hash data))
    |> Result.map (printfn "%s")