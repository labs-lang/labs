module Parser

open FParsec
open Common
open Components
open System
open Stigmergies
open Properties
 
let parse =
    ws_ >>. tuple4
        (ws psys) 
        ((plstig |> ws |> many))
        ((pcomp |> ws |> many))
        (pproperties <!> "PROPERTIES")
    <!> "PARSER"