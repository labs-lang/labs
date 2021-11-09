module Parser

open FParsec
open Common
open Components
open System
open Stigmergies
open Properties
 
let parse =
    wsUnit >>. tuple5
        (ws psys) 
        ((plstig |> ws |> many) <!> "STIGMERGIES")
        ((pcomp |> ws |> many) <!> "AGENTS")
        (((opt passume) |> ws) <!> "ASSUME")
        (pproperties <!> "PROPERTIES")
    <!> "PARSER"