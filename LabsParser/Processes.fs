module Processes

open FParsec
open Types
open Expressions    


/// Parses elementary processes ("actions")
let paction : Parser<_> =
    let parseArrow : Parser<_> =
        stringReturn ":=" EnvWrite
        <|>
        (skipChar '<' >>.
        choice [
            charReturn '-' AttrUpdate;
                charReturn '~' LStigUpdate;])
    (pipe3 
        (ws KEYNAME) 
        (ws parseArrow) 
        (ws pexpr) 
        (fun k action e -> action(k, e)))

let pproc, pprocRef = createParserForwardedToRef()
let pprocTerm, pprocTermRef = createParserForwardedToRef()

do pprocTermRef :=
    let pNil = stringReturn "Nil" Nil
    let pSkip = stringReturn "Skip" Skip
    choice [
        attempt pNil; 
        attempt pSkip; 
        IDENTIFIER |>> Process.Name; 
        paction |>> Base; 
        betweenParen pproc
    ]

do pprocRef := 
    // Returns a Process type from the corresponding char
    let OP : Parser<_> = 
        choice [
            (charReturn '&' (^+));
            (charReturn '|' (^|));
            (charReturn ';' (^.));
        ]
    // Either returns a single term, or creates a choice/par/seq
    // from two processes
    maybeTuple2 (ws pprocTerm) ((ws OP) .>>. (ws pproc)) (fun (a, (b, c)) -> b a c)

let pdef = 
    (ws IDENTIFIER) .>>. (ws (ws EQ) >>. (ws pproc))

let processes = 
    many (pdef .>> manyComments)
    >>= (fun x -> fun _ -> 
        let dup = x |> List.map fst |> List.duplicates 
        if dup.Length > 0 then
            dup
            |> String.concat ", "
            |> sprintf "These processes have multiple definitions: %s"
            |> (fun msg -> Reply(Error, ErrorMessageList(Message(msg))))
        else Reply(x |> Map.ofList))