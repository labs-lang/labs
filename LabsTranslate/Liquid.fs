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

let fs = new FileSystems.LocalFileSystem("")

let private internalRender strfun vals (template:Template) =
    let rec hashval = function
        | Int i -> box i
        | Bool b -> box b
        | Str s -> box s
        | Lst l -> l |> Seq.map hashval |> box
        | Dict x -> hashdict x |> box
        and hashdict x = 
            Seq.map (fun (k, v) -> k, (hashval v)) x
            |> dict
            |> Hash.FromDictionary
    let render = template.Render (hashdict vals)
    if template.Errors.Count = 0 then 
        Result.Ok (strfun render)
    else 
        template.Errors
        |> Seq.map (fun x -> x.Message)
        |> String.concat "\n"
        |> sprintf "Code generation failed with the following message:\n%s"
        |> Result.Error

/// Renders a given template to standard output
let render v t = internalRender (printfn "%s") v t

/// Renders a given template to string
let strRender v t = internalRender id v t

let parse path =
    Template.FileSystem <- fs
    readFile path
    |> Result.map Template.Parse

///<summmary>Opens a template file and renders it using the specified
///local variables.</summary>
let renderFile path (vals:LiquidDict) =
    parse path
    |> Result.bind (strRender vals)

// Reusable templates, we only parse them once
let goto = parse "templates/goto.c"
let transition = parse "templates/transition.c"
let stop = parse "templates/stop.c"