namespace Buzz
open System
    [<AutoOpen>]    
    module Types = 
        type Point = int * int

        type AttrVal =
            | Int of int
            | String of string
            | P of Point

        type Val = int
        type Tval = Val * DateTime

        type Key = string
        type Tpair = Key * Tval

        type Interface = Map<string, AttrVal>

        type Action =
            | Attr of string * AttrVal
            | Put of Tpair
            | Await of Key * Val
            override this.ToString() = 
                match this with
                | Attr(a, v) -> sprintf "[%s := %s]" a (v.ToString())
                | Put(p) -> sprintf "{%s <- %A}" (fst p) (fst (snd p))
                | Await(k, v) -> sprintf "<%A = %A>" k v
        
        [<StructuredFormatDisplay("{AsString}")>]
        type Process = 
            | Nil
            | Proxy of string * Lazy<Process>
            | Prefix of Action * Process
            static member ( ^. )(left: Action, right: Process) =
                Prefix(left, right)
            member this.AsString = this.ToString()        
            override this.ToString() =
                match this with
                | Nil -> "0"
                | Proxy(name, p) -> name 
                | Prefix(action, proc) -> sprintf "%s.%s" (action.ToString()) proc.AsString
