module Processes

open FParsec
open Buzz.Types
open Expressions    

let paction : Parser<_> =
    let pactionAttr = 
        interfaceKey .>>. (betweenspaces ASSIGN >>. pexpr) |>> AttrUpdate
    let pactionLstig =
        lstigKey .>>. (betweenspaces ASSIGN >>. pexpr) |>> LStigUpdate
    let pactionAwait =
        pbexpr .>> AWAIT |>> Await
    choice [pactionAwait; pactionAttr; pactionLstig]

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pprocNil = stringReturn "0" Nil
    let pprocParen = between (pchar '(') (pchar ')') pproc
    let pprocSeq =
        paction .>>. (PREFIX >>. pprocTerm) |>> Process.Prefix
    choice [pprocSeq; pprocParen; pprocNil]

let pprocChoice : Parser<_> =
    maybeTuple2 pprocTerm (betweenspaces (pchar '+') >>. pproc) Process.Choice

do pprocRef := choice [pprocChoice; pprocTerm]