module internal Liquid

open Base
open DotLiquid

let makehash (l: ('a * 'b) list) =
        l
        |> List.map (fun (a,b) ->
            dict ["name", box a; "value", box b]
            |> Hash.FromDictionary)
        |> box

///<summmary>Opens a template file and renders it using the specified
///local variables.</summary>
let renderFile path (data:(string * obj) list) = 
    readFile(path)
    |> Result.map Template.Parse
    |> Result.map (fun x -> x.Render (data |> dict |> Hash.FromDictionary))
    |> Result.map (printfn "%s")