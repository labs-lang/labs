module internal Liquid

open Base
open DotLiquid

type LiquidDict =
    (string * LiquidVal) seq
and LiquidVal = 
    | Str of string
    | Int of int
    | Bool of bool
    | Lst of LiquidVal seq
    | Dict of LiquidDict

///<summmary>Opens a template file and renders it using the specified
///local variables.</summary>
let renderFile path (vals:LiquidDict) =
    let rec hashval = function
    | Int(i) -> box i
    | Bool(b) -> box b
    | Str(s) -> box s
    | Lst(l) -> l |> Seq.map hashval |> box
    | Dict(x) -> hashdict x |> box
    and hashdict x = 
        Seq.map (fun (k,v) -> k, (hashval v)) x
        |> dict
        |> Hash.FromDictionary

    readFile(path)
    |> Result.map Template.Parse
    |> Result.map (fun x -> x, x.Render (hashdict vals))
    // Return a Eesult.Error if DotLiquid cannot render the template
    |> Result.bind(
        fun (x,y) ->
            if x.Errors.Count = 0 then Result.Ok (x,y)
            else 
                x.Errors
                |> Seq.map (fun x -> x.Message )
                |> String.concat "\n"
                |> sprintf "%s: Code generation failed with the following message:\n%s" path
                |> Result.Error
        )
    // If no error, print the rendered template
    |> Result.map (fun (_,y) -> printfn "%s" y)