[<AutoOpen>]
module internal Common
open FParsec
open Types

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let COMMENT : Parser<_> =       (skipChar '#')
let COLON : Parser<_> =         (skipChar ':')
let COMMA : Parser<_> =         (skipChar ',')
let EQ : Parser<_> =            (skipChar '=')
let GUARD : Parser<_> =         (skipString "->")
let NEG : Parser<_> =           (skipChar '!')
let RANGE : Parser<_> =         (skipString "..")
let strUNDEF =                  "undef"
//-----------------------------------------

let isAlphanum x = isAsciiLetter x || isDigit x

let KEYNAME : Parser<_> =
    IdentifierOptions(
        isAsciiIdStart=isAsciiLower,
        isAsciiIdContinue=isAlphanum)
    |> identifier

let IDENTIFIER : Parser<_> = 
    IdentifierOptions(
        isAsciiIdStart=isAsciiUpper,
        isAsciiIdContinue=isAlphanum)
    |> identifier

let withcommas x = x |> Seq.map (sprintf "%O") |> String.concat ", "

/// Parse p and skip whitespace after.
let ws p = p .>> spaces
let betweenBrackets p = between (ws (skipChar '[')) (skipChar ']') p
let betweenBraces p = between (ws (skipChar '{')) (skipChar '}') p
let betweenParen p = between (ws (skipChar '(')) (skipChar ')') p
let betweenAng p = between (ws (skipChar '<')) (skipChar '>') p
let sepbycommas p = sepBy1 p (ws COMMA)
let sepbysemis p = sepBy1 p (ws (skipChar ';'))

/// Helper for tracing parsers
// http://www.quanttec.com/fparsec/users-guide/debugging-a-parser.html
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    #if DEBUG
    fun stream ->
        eprintfn "%A: Entering %s" stream.Position label
        let reply = p stream
        eprintfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply
    #else
    p
    #endif

/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2 <!> "p2") |>> 
    function
    | a, Some b -> if2(a, b)
    | a, None -> a

let lineComment : Parser<_> = COMMENT >>. skipRestOfLine false
    
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

let pvar loc = 
    pipe2 KEYNAME (opt (betweenBrackets puint32))
        (fun name -> 
            let v = {vartype=Scalar; name=name; location=loc; init=Undef}
            function
            | Some b -> {v with vartype=Array(int b)}
            | None -> v)

let pinit = 
    let pChoose = 
        (sepbycommas pint32) |> betweenBrackets |>> Choose
    let pRange = 
        followedBy (pint32 >>. RANGE)
        >>. ((ws pint32) .>>. (RANGE >>. (ws pint32)) 
        |>> Range)
    let pSingle = (ws pint32) |>> (Choose << List.singleton)
    let pUndef = stringReturn strUNDEF Undef

    choice [pChoose; pRange; pSingle; pUndef] |> ws

/// Parses a single init definition.
let pinitdef loc =
    (pvar loc) .>>. ((ws COLON) >>. pinit)
    |>> fun (var, init) -> {var with init=init}

let inline byName v =
   (^T : (member name : string) v)

let pkeys loc = 
    //let lbl = function I -> "interface" | L _ -> "stigmergy" | E -> "environment"
    ws (sepbysemis (ws (pinitdef loc)))
    >>= toSet byName byName 

let pstringEq str p = 
    (ws (skipString str) >>. (ws EQ) >>. p)