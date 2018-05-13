[<AutoOpen>]
module internal Common
open FParsec

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let INTERFACE : Parser<_> =     (skipChar 'I')
let LSTIG : Parser<_> =         (skipChar 'L')
let ENV : Parser<_> =           (skipChar 'E')
let NEG : Parser<_> =           (skipChar '!')
let CONJ : Parser<_> =          (skipChar '&')
let EQ : Parser<_> =            (skipChar '=')
let SEQ : Parser<_> =           (skipChar ';')
let PAR : Parser<_> =           (skipChar '|')
let CHOICE : Parser<_> =        (skipChar '+')
let COMMENT : Parser<_> =       (skipChar '#')
//-----------------------------------------



let isAlphanum x = isAsciiLetter x || isDigit x
let isAlphanumlower x = isAsciiLower x || isDigit x

let KEYNAME : Parser<_> =
    asciiLower .>>. (manySatisfy isAlphanumlower)
    |>> (fun (x, y) -> (string x) + y)

let IDENTIFIER : Parser<_> = 
    asciiUpper .>>. (manySatisfy isAlphanum)
    |>> (fun (x, y) -> (string x) + y)

/// Parse p and skip whitespace after.
let ws p = p .>> spaces
let btw beginchar endchar : Parser<_> = 
    between (pchar beginchar) (pchar endchar) (manySatisfy ((<>) endchar))
let betweenBrackets p = between (pchar '[') (pchar ']') p
let betweenParen = btw '(' ')'
let betweenQuotes = btw '"' '"'
let betweenspaces p = between spaces spaces p
let andspaces p = p >>. spaces


/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2) |>> function
    | a, Some(b) -> if2(a, b)
    | a, None -> a
