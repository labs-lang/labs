module LabsToC.Liquid

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
        |> Seq.map (fun x -> {What=Codegen x.Message; Where=[]})
        |> (Seq.toList >> wrap (strfun "") [])

/// Renders a given template to standard output
let render template values = internalRender (printfn "%s") template values

/// Renders a given template to a string.
let strRender template values = internalRender id template values

let parse path =
    Environment.CurrentDirectory <- System.AppDomain.CurrentDomain.BaseDirectory
    Template.FileSystem <- fs
    File.ReadAllText path
    |> Template.Parse

///<summmary>Opens a template file and renders it using the specified
///local variables.</summary>
let renderFile path (vals:LiquidDict) =
    (strRender (parse path) vals)

let makeDict typeofName typeofValue =
    Lst << Seq.map (fun (a, b) -> Dict ["name", typeofName a; "value", typeofValue b])

let liquidPcs pcset =
    pcset
    |> Map.toSeq |> makeDict Int (Lst << (Seq.map Int))