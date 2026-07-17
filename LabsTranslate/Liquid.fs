module LabsTranslate.Liquid

open System
open System.IO
open System.Text.RegularExpressions
open Frontend.Outcome
open Frontend.Message
open Scriban
open Scriban.Runtime

type LiquidDict = (string * LiquidVal) seq

and LiquidVal =
    | Str of string
    | Int of int
    | Bool of bool
    | Lst of LiquidVal seq
    | Dict of LiquidDict

/// Wrapper for arrays that supports Scriban member access (.first, .size),
/// iteration (for loops), and renders single-element arrays as their scalar value
/// (matching DotLiquid behavior).
type LiquidArray(items: obj[]) =
    member _.Items = items
    member _.first = if items.Length > 0 then items.[0] else null
    member _.size = items.Length
    interface System.Collections.IEnumerable with
        member _.GetEnumerator() = (items :> System.Collections.IEnumerable).GetEnumerator()
    interface IFormattable with
        member _.ToString(_format, _provider) =
            if items.Length = 1 then
                match items.[0] with
                | :? IFormattable as f -> f.ToString(null, Globalization.CultureInfo.InvariantCulture)
                | x -> string x
            else items |> Array.map string |> String.concat ", "
    override _.ToString() =
        if items.Length = 1 then string items.[0]
        else items |> Array.map string |> String.concat ", "

let rec toObject =
    function
    | Int i -> box i
    | Bool b -> box b
    | Str s -> box s
    | Lst l -> LiquidArray(l |> Seq.map toObject |> Seq.toArray) :> obj
    | Dict x ->
        let so = ScriptObject()
        for k, v in x do
            so.Add(k, toObject v)
        so :> obj

let private internalRender strfun (template: Template) (values: LiquidDict) =
    let so = ScriptObject()
    for k, v in values do
        so.Add(k, toObject v)

    try
        let result = template.Render(so)
        zero (strfun result)
    with ex ->
        [{ What = Codegen ex.Message
           Where = [ Position("template", -1, -1, -1) ] }]
        |> wrap (strfun "") []

/// Renders a given template to standard output
let render template values =
    internalRender (printfn "%s") template values

/// Renders a given template to a string.
let strRender template values = internalRender id template values

let private exeDir =
    Diagnostics.Process.GetCurrentProcess().MainModule.FileName
    |> Path.GetDirectoryName

/// Scriban requires a space inside {%- and -%} tags; DotLiquid did not.
/// Normalize tags so that `{%-if` becomes `{%- if` and `endif-%}` becomes `endif -%}`.
let private normalizeLiquidTags (text: string) =
    text
    |> fun s -> Regex.Replace(s, @"\{%-([^\s])", "{%- $1")
    |> fun s -> Regex.Replace(s, @"([^\s])-%}", "$1 -%}")

let parse path =
    let fullPath =
        if Path.IsPathRooted(path : string) then path
        else Path.Combine(exeDir, path)
    let text = File.ReadAllText fullPath |> normalizeLiquidTags
    Template.ParseLiquid(text, path)

///<summary>Opens a template file and renders it using the specified local variables.</summary>
let renderFile path (vals: LiquidDict) = (strRender (parse path) vals)

/// Turns a sequence of pairs into a Liquid dictionary.
let makeDict typeofName typeofValue =
    Lst
    << Seq.map (fun (a, b) -> Dict [ "name", typeofName a; "value", typeofValue b ])

/// Helper to make Liquid dictionaries out of program counters
let liquidPcs pcset =
    pcset |> Map.toSeq |> makeDict Int (Lst << Seq.map Int)
