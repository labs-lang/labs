[<AutoOpen>]
module internal Common
open FParsec

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let INTERFACE : Parser<_> =     (pchar 'I')
let LSTIG : Parser<_> =         (pchar 'L')
let NEG : Parser<_> =           (pchar '!')
let CONJ : Parser<_> =          (pchar '&')
let ASSIGN : Parser<_> =        (pstring ":=")
let AWAIT : Parser<_> =         (pchar '?')
let PREFIX : Parser<_> =        (pchar '.')
//-----------------------------------------

let ws = spaces
let btw beginchar endchar : Parser<_> = 
    between (pchar beginchar) (pchar endchar) (manySatisfy ((<>) endchar))
let betweenBrackets = btw '[' ']'
let betweenParen = btw '(' ')'
let betweenQuotes = btw '"' '"'
let betweenspaces p = between spaces spaces p
let andspaces p = p >>. spaces

let interfaceKey = INTERFACE >>. betweenBrackets
let lstigKey = LSTIG >>. betweenBrackets

/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2) |>> function
    | a, Some(b) -> if2(a, b)
    | a, None -> a
