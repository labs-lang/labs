module Processes

open FParsec
open Types
open Expressions    


let pEnvWrite =
        envKey .>>. (betweenspaces ASSIGN >>. pexpr) |>> EnvWrite 

/// Parses elementary processes ("actions")
let paction : Parser<_> =
    // Attributes can be set either to expression or environment values
    let pactionAttr = 
        let envOrExpr k = 
           (followedBy envKey >>. envKey |>> fun x -> EnvRead(k, x))
           <|>
           (pexpr |>> fun e -> AttrUpdate(k, e))

        let key = interfaceKey .>> (betweenspaces ASSIGN)
        key >>= envOrExpr

    let pEnvRead =
        interfaceKey .>>. (betweenspaces ASSIGN >>. envKey) |>> EnvRead 
     
    let pactionLstig =
        lstigKey .>>. (betweenspaces ASSIGN >>. pexpr) |>> LStigUpdate

    choice [pactionLstig; pEnvWrite; pactionAttr]

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pNil = stringReturn "0" Nil
    let pTick = stringReturn "1" Tick
    let pParen = between (pchar '(') (pchar ')') pproc
    //let pSeq =  
    //    printf "prova pSeq"
    //    pprocTerm .>>. (SEQ >>. ws >>. pproc) |>> Process.Seq
    //let pPar =
    //    pprocTerm .>>. (PAR >>. ws >>. pproc) |>> Process.Par
    //let pChoice =
        //pprocTerm .>>. (CHOICE >>. ws >>. pproc) |>> Process.Choice
    let pName =
        IDENTIFIER .>> notFollowedBy (pchar '[') |>> Process.Name
    choice [attempt pName; pNil; pTick; paction |>> Base; pParen]

do pprocRef := 
    // Returns a Process type from the corresponding char
    let OP : Parser<_> = 
        choice [
            (charReturn '&' Process.Choice);
            (charReturn '|' Process.Par);
            (charReturn ';' Process.Seq);
        ]
    // Either returns a single term, or creates a choice/par/seq
    // from two processes
    maybeTuple2 (ws pprocTerm) ((ws OP) .>>. (ws pproc)) (fun (a, (b, c)) -> b a c)