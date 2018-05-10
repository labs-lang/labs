[<AutoOpen>]
module internal Common
open FParsec

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let INTERFACE : Parser<_> =     (pchar 'I')
let LSTIG : Parser<_> =         (pchar 'L')
let ENV : Parser<_> =           (pchar 'E')
let NEG : Parser<_> =           (pchar '!')
let CONJ : Parser<_> =          (pchar '&')
let ASSIGN : Parser<_> =        (pstring ":=")
let AWAIT : Parser<_> =         (pchar '?')
let SEQ : Parser<_> =           (pchar ';')
let PAR : Parser<_> =           (pchar '|')
let CHOICE : Parser<_> =        (pchar '+')
//-----------------------------------------

let IDENTIFIER : Parser<_> = (regex "[A-Z][A-Za-z0-9]*")
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
let envKey = ENV >>. betweenBrackets

/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2) |>> function
    | a, Some(b) -> if2(a, b)
    | a, None -> a
