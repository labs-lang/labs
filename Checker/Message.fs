module Checker.Message

type Position = FParsec.Position

type Err =
    | Duplicate of string
    | UndefProcess of string
    | UndefRef of string
    | UndefBehavior of string
    | UndefAgent of string
    | NoValueForExtern of string
    | NegativeSpawn of string
    | Generic of string

type Warn =
    | SpawnZero of string
    | Unused of string //TODO

type Message<'a> =
    {
        what: 'a
        where: Position list
    }

exception LabsException of Message<Err>    

let map f (d:Node<_>) =
    try {pos=d.pos; name=d.name; def=f d.def}
    with :? LabsException as e -> raise (LabsException {e.Data0 with where=[d.pos]})
   
