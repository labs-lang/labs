[<AutoOpen>]
module internal Common

open Tokens
open FParsec

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let COMMENT : Parser<_> =       (skipString tCOMMENT)
let COLON : Parser<_> =         (skipChar ':')
let COMMA : Parser<_> =         (skipChar ',')
let EQ : Parser<_> =            (skipChar '=')
let RANGE : Parser<_> =         (skipString "..")
let OF : Parser<_> =            (skipString "of")
let GUARD : Parser<_> =         (skipString tGUARD)
let NEG : Parser<_> =           (skipString tNEG)
let SEQ : Parser<_> =           (skipString tSEQ)
let CHOICE : Parser<_> =        (skipString tCHOICE)
let PAR : Parser<_> =           (skipString tPAR)
//-----------------------------------------


let lineComment : Parser<_> = COMMENT >>. skipRestOfLine false

/// Parse p and skip whitespace/comments after.
let ws p = p .>> spaces .>> skipMany (spaces1 <|> lineComment)
let ws_ = ws (preturn ())

let isAlphanum x = isAsciiLetter x || isDigit x
let notInIdentifier : Parser<_> = notFollowedBy (satisfy isAlphanum)

// Parses reserved keyword so they are not parsed as identifiers or names
let reserved : Parser<_> = 
    [tTRUE; tFALSE; tCONJ; tDISJ; tABS; tID; tMIN; tMAX]
    |> List.map pstring 
    |> choice
    .>> notInIdentifier

let safeSkip str = skipString str .>> notInIdentifier
let safeStrReturn str ret = safeSkip str >>% ret

let safeIdentifier options =
    (notFollowedBy reserved >>. identifier options)
    <|> (followedBy reserved >>. reserved >>= (fail << (sprintf "Unexpected keyword '%s'")))
    
let KEYNAME : Parser<_> =
    IdentifierOptions(
        isAsciiIdStart=isAsciiLower,
        isAsciiIdContinue=isAlphanum)
    |> safeIdentifier

let IDENTIFIER : Parser<_> = 
    IdentifierOptions(
        isAsciiIdStart=isAsciiUpper,
        isAsciiIdContinue=isAlphanum)
    |> safeIdentifier

let withcommas x = x |> Seq.map (sprintf "%O") |> String.concat ", "

let enclose startc endc = between (ws (skipChar startc)) (skipChar endc)

let betweenBrackets p = enclose '[' ']' p
let betweenBraces p = enclose '{' '}' p
let betweenParen p = enclose '(' ')' p
let betweenAng p = enclose '<' '>' p
let sepbycommas p = sepBy1 p (ws COMMA)
let sepbysemis p = sepBy1 p (ws (skipChar ';'))

/// Helper for tracing parsers
// http://www.quanttec.com/fparsec/users-guide/debugging-a-parser.html
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    #if DEBUG
    fun stream ->
        eprintfn "%A: Entering %s (char: %O)" stream.Position label (stream.Peek())
        let reply = p stream
        eprintfn "%A: Leaving %s (%A, %O)" stream.Position label reply.Status reply.Result
        reply
    #else
    p
    #endif
    
let toMapF formatonFail lst =
    let dup = lst |> List.map fst |> List.duplicates
    if dup.Length > 0 then
        dup
        |> List.map formatonFail
        |> withcommas 
        |> sprintf "Multiple definitions of %s"
        |> fun msg _ -> Reply(Error, unexpected msg)
    else
        preturn (lst |> Map.ofList)

let toMap lst = toMapF id lst

let toSet dupFn formatOnFail lst = 
    let dup = List.duplicatesBy dupFn lst
    if dup.Length > 0 then 
        dup
        |> List.map formatOnFail
        |> withcommas 
        |> sprintf "Multiple definitions of %s"
        |> fun msg _ -> Reply(Error, unexpected msg)
    else
        preturn (Set.ofList lst)
        
let inline byName v = (^T : (member name : string) v)

let pstringEq str p = 
    (ws (skipString str) >>. (ws EQ) >>. p)
    
let pextern : Parser<_> = ((skipChar '_') >>. (KEYNAME))