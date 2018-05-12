module Parser

open FParsec
open Types

let private pProc = Processes.pproc

type private Line =
    | Def of string * Process
    | Comment of unit

let private lineComment : Parser<_> = (ws COMMENT) >>. skipRestOfLine false

let private pdef = 
    (ws IDENTIFIER) .>>. (ws (skipChar '=') >>. (ws pProc)) .>> optional lineComment

let private processes = 
    (spaces >>. many ((pdef |>> Def) <|> (lineComment |>> Comment)) |> ws)
    |>> List.choose (function Def(a,b) -> Some (a,b) | _ -> None)

let parse = processes