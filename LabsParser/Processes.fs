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

let precprocTerm, precprocTermRef = createParserForwardedToRef()
let precproc, precprocRef = createParserForwardedToRef()

do precprocTermRef :=
    choice [
        stringReturn "0" RNil;
        stringReturn "X" X;
        between (pchar '(') (pchar ')') precproc;
        paction .>>. (PREFIX >>. precprocTerm) |>> RPrefix
    ]

do precprocRef :=
    maybeTuple2 precprocTerm (betweenspaces (pchar '+') >>. precproc) RChoice
    

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pprocNil = stringReturn "0" Nil
    let pprocRec = (pstring "recX.") >>. precproc |>> Process.RecX
    let pprocParen = between (pchar '(') (pchar ')') pproc
    let pprocSeq =
        paction .>>. (PREFIX >>. pprocTerm) |>> Process.Prefix
    choice [pprocNil; pprocRec; pprocParen; pprocSeq]

do pprocRef := 
    maybeTuple2 pprocTerm (betweenspaces (pchar '+') >>. pproc) Process.Choice