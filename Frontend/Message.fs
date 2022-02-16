module Frontend.Message
open LabsCore.Grammar

type Position = FParsec.Position

type Err =
    | Parser of string
    | Duplicate of string
    | UndefProcess of string
    | UndefRef of string
    | UndefBehavior of string
    | UndefAgent of string
    | NoValueForExtern of string
    | NegativeSpawn of string
    | NonPositiveArraySize of string
    | Codegen of string
    | Generic of string
    | CLI of string

type Warn =
    | SpawnZero of string
    | Unused of string //TODO

type Message<'a> =
    {
        What: 'a
        Where: Position list
    }

let private _pprintWithPos header msg (where: Position list) =
    let lineColumn (p: Position) = if p.Line > 0 then $":{p.Line}:{p.Column}" else ""
    let pos = where |> List.map (fun p -> $"{p.StreamName}{lineColumn p}") |> String.concat "; "
    $"[%s{header}] %s{msg} at %s{pos}"
    
let pprintErr (m:Message<Err>) =
    match m.What with
        | Parser s -> $"Parser failed: {s}"
        | Duplicate s -> $"Duplicate definitions for '{s}'"
        | UndefProcess s -> $"Process '{s}' was not defined"
        | UndefAgent s -> $"Agent '{s}' was not defined"
        | UndefBehavior s -> $"Behavior of agent '{s}' was not defined"
        | UndefRef s -> $"Identifier '{s}' was not defined"
        | NegativeSpawn s -> $"Cannot spawn a negative number of agents '{s}'"
        | NoValueForExtern s -> $"No value was given for extern parameter '{s}'"
        | NonPositiveArraySize s -> $"Array '{s}' must have positive size"
        | Codegen s -> $"Code generation failed: {s}"
        | CLI s -> $"Parsing of the command line failed: {s}"
        | Generic s -> s
    |> fun msg -> _pprintWithPos "ERROR" msg m.Where

let pprintWarn (m:Message<Warn>) =
    match m.What with
        | SpawnZero s -> $"Agent '{s}' has spawn size 0 and will not be spawned."
        | Unused s -> $"Unused: '{s}'"
    |> fun msg -> _pprintWithPos "WARNING" msg m.Where

exception LabsException of Message<Err>

let map f (d:Node<_>) =
    try {Pos=d.Pos; Name=d.Name; Def=f d.Def; Source=d.Source}
    with :? LabsException as e -> raise (LabsException {e.Data0 with Where=[d.Pos]})
    
