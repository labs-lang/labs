module Parser

open FParsec
open Types

let private pProc = Processes.pproc

type private Line =
    | Def of string * Process
    | Comment of unit

let lineComment : Parser<_> = (skipChar '#') >>. skipRestOfLine false .>> ws

let pdef = 

    (IDENTIFIER .>> ws ) .>>. (pchar '=' >>. ws >>. pProc) .>> ws

let full = 
    (ws >>. many ((pdef |>> Def) <|> (lineComment |>> Comment)) .>> (ws .>> eof))
    |>> List.choose (function Def(a,b) -> Some (a,b) | _ -> None)

let pInit = Processes.pInit