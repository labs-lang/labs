[<AutoOpen>]
module internal Common
open FParsec

open LabsCore.Tokens
open LabsCore.ExprTypes

type Parser<'t> = Parser<'t, unit>

// Token parsers
//-----------------------------------------
let COMMENT : Parser<_> =       (skipString tCOMMENT)
let COLON : Parser<_> =         (skipChar ':')
let COMMA : Parser<_> =         (skipChar ',')
let EQ : Parser<_> =            (skipChar '=')
let RANGE : Parser<_> =         (skipString "..")
let OF : Parser<_> =            (skipString tOF)
let GUARD : Parser<_> =         (skipString tGUARD)
let NEG : Parser<_> =           (skipString tNEG)
let SEQ : Parser<_> =           (skipString tSEQ)
let CHOICE : Parser<_> =        (skipString tCHOICE)
let PAR : Parser<_> =           (skipString tPAR)
let RAWPREFIX : Parser<_> =     (skipString tRAW)
//-----------------------------------------


let lineComment : Parser<_> = COMMENT >>. skipRestOfLine false

/// Parses p and skips whitespace/comments after.
let ws p = p .>> spaces .>> skipMany (spaces1 <|> lineComment)
/// Parses whitespace immediately
let wsUnit = ws (preturn ())

let isIdentifierChar x = isAsciiLetter x || isDigit x || x = '_'
let notInIdentifier : Parser<_> = notFollowedBy (satisfy isIdentifierChar)

let safeSkip str = skipString str .>> notInIdentifier
let safeStrReturn str ret = safeSkip str >>% ret

let safeIdentifier options =
    let reserved = [
        tABS; tCONJ; tDISJ; tELSE; tFALSE;
        tID; tIF; tMAX; tMIN; tOF; tPICK
        tSKIP; tTHEN; tTRUE; tUNDEF; tWHERE
        "forall"; "exists"; "count"
    ]
    identifier options
    >>= fun x ->
        if List.contains x reserved
        then fail $"Unexpected keyword '{x}'"
        else preturn x

let KEYNAME : Parser<_> =
    IdentifierOptions(isAsciiIdStart=isAsciiLower)
    |> safeIdentifier

let IDENTIFIER : Parser<_> = 
    IdentifierOptions(isAsciiIdStart=isAsciiUpper)
    |> safeIdentifier


/// Helper function, returns string with elements of x separated by commas
let private toCommaSepString x = x |> Seq.map (sprintf "%O") |> String.concat ", "

/// Helper parser: a "between" with optional whitespace after startc
let private enclose startc endc = between (ws (skipChar startc)) (skipChar endc)

let betweenBrackets p = enclose '[' ']' p
let betweenBraces p = enclose '{' '}' p
let betweenBracesPos p = getPosition .>>. betweenBraces p
let betweenParen p = enclose '(' ')' p
let betweenAng p = enclose '<' '>' p
let sepbycommas p = sepBy1 p (ws COMMA)
let sepbysemis p = sepBy1 p (ws (skipChar ';'))

/// Helper for tracing parsers
// http://www.quanttec.com/fparsec/users-guide/debugging-a-parser.html
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    #if DEBUG
    fun stream ->
        eprintfn $"%A{stream.Position}: Entering %s{label} (char: {stream.Peek()})"
        let reply = p stream
        eprintfn $"%A{stream.Position}: Leaving %s{label} (%A{reply.Status}, {reply.Result})"
        reply
    #else
    p
    #endif
    
/// Turns a list of key-value pairs into a map.
/// Fails if there are duplicate keys.
let toMap lst =
    let toMapF formatonFail =
        let dup = lst |> List.map fst |> List.duplicates
        if dup.Length > 0 then
            dup
            |> List.map formatonFail
            |> toCommaSepString 
            |> sprintf "Multiple definitions of %s"
            |> fun msg _ -> Reply(Error, unexpected msg)
        else
            preturn (lst |> Map.ofList)
    toMapF id

/// Turns a list of values into a set.
let toSet dupFn formatOnFail lst = 
    let dup = List.duplicatesBy dupFn lst
    if dup.Length > 0 then 
        dup
        |> List.map formatOnFail
        |> toCommaSepString 
        |> sprintf "Multiple definitions of %s"
        |> fun msg _ -> Reply(Error, unexpected msg)
    else
        preturn (Set.ofList lst)

/// Parses "<str> = <p>" and returns <p>
let pstringEq str p = 
    (ws (skipString str) >>. (ws EQ) >>. p)
    
/// Parses an external parameter name
let pextern : Parser<_> = ((skipChar '_') >>. KEYNAME)

let pquantifier =
    pipe3
        (ws <| choice [
            stringReturn "forall" All;
            stringReturn "exists" Exists
        ])
        (ws IDENTIFIER)
        (ws KEYNAME)
        (fun a b c -> c, (b, a))