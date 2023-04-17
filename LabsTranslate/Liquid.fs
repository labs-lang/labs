module LabsTranslate.Liquid

open System
open Frontend.Outcome
open Frontend.Message
open DotLiquid
open System.IO

type LiquidDict =
    (string * LiquidVal) seq
and LiquidVal = 
    | Str of string
    | Int of int
    | Bool of bool
    | Lst of LiquidVal seq
    | Dict of LiquidDict

let fs = FileSystems.LocalFileSystem("")

let private internalRender strfun (template:Template) values =



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
    let render = template.Render (hashdict values)
    if template.Errors.Count = 0 then 
        zero (strfun render)
    else
        template.Errors
        |> Seq.map (fun x ->
            {What=Codegen x.Message; Where=[Position(x.Source,-1,-1,-1)]})
        |> (Seq.toList >> wrap (strfun "") [])

/// Renders a given template to standard output
let render template values = internalRender (printfn "%s") template values

/// Renders a given template to a string.
let strRender template values = internalRender id template values

let parse path =
    Environment.CurrentDirectory <-
        Diagnostics.Process.GetCurrentProcess().MainModule.FileName 
        |> Path.GetDirectoryName
    Template.FileSystem <- fs
    try
        File.ReadAllText path
        |> Template.Parse
    with
    | ex -> failwith $"Parsing failed: {path}: {ex.Message}"

///<summary>Opens a template file and renders it using the specified local variables.</summary>
let renderFile path (vals:LiquidDict) =
    (strRender (parse path) vals)

/// Turns a sequence of pairs into a Liquid dictionary.
let makeDict typeofName typeofValue =
    Lst << Seq.map (fun (a, b) -> Dict ["name", typeofName a; "value", typeofValue b])

/// Helper to make Liquid dictionaries out of program counters
let liquidPcs pcset =
    pcset
    |> Map.toSeq |> makeDict Int (Lst << (Seq.map Int))