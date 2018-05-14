module Parser

open FParsec
open Types
open Functions
open Processes

type private Line =
    | Def of string * Process
    | Comment of unit

let private lineComment : Parser<_> = (ws COMMENT) >>. skipRestOfLine false

let private definition str = 
    (ws (skipString str)) >>.
    (ws EQ) >>. (sepBy KEYNAME (ws <| skipChar ','))

let private setDef str = (definition str) >>= (fun x -> fun _ -> 
    let dup = duplicates x
    if dup.Length > 0 then
        dup
        |> String.concat ", "
        |> sprintf "%s: keys defined more than once: %s" str
        |> (fun msg -> Reply(Error, ErrorMessageList(Message(msg))))
    else Reply(x |> Set.ofList))
    

let private psys = 
    (skipMany lineComment) >>.
    (pipe4
    (setDef "interface") 
    (setDef "stigmergy") 
    (setDef "environment")
    (definition "system")
    (fun a b c d -> {iface = a; lstig = b; environment = c; components = d}))

let private pdef = 
    (ws IDENTIFIER) .>>. (ws (ws EQ) >>. (ws pproc)) .>> skipMany lineComment

let private processes = 
    (spaces >>. many ((pdef |>> Def) <|> (lineComment |>> Comment)) |> ws)
    |>> List.choose (function Def(a,b) -> Some (a,b) | _ -> None)

let parse = psys .>>. processes