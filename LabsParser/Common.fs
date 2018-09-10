[<AutoOpen>]
module internal Common
open FParsec
open Types

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let NEG : Parser<_> =           (skipChar '!')
let CONJ : Parser<_> =          (skipChar '&')
let EQ : Parser<_> =            (skipChar '=')
let COMMENT : Parser<_> =       (skipChar '#')
let COLON : Parser<_> =         (skipChar ':')
//-----------------------------------------

let isAlphanum x = isAsciiLetter x || isDigit x
let isAlphanumlower x = isAsciiLower x || isDigit x

let KEYNAME : Parser<_> =
    pipe2 asciiLower (manySatisfy isAlphanumlower)
        (fun x y -> sprintf "%O%s" x y)

let IDENTIFIER : Parser<_> = 
    pipe2 asciiUpper (manySatisfy isAlphanum) (fun x y -> sprintf "%O%s" x y)

let withcommas x = x |> Seq.map (sprintf "%A") |> String.concat ", "

/// Parse p and skip whitespace after.
let ws p = p .>> spaces
let betweenBrackets p = between (ws (skipChar '[')) (skipChar ']') p
let betweenBraces p = between (ws (skipChar '{')) (skipChar '}') p
let betweenParen p = between (ws (skipChar '(')) (skipChar ')') p
let betweenAng p = between (ws (skipChar '<')) (skipChar '>') p
let sepbycommas p = sepBy p (ws (skipChar ','))

/// Helper for tracing parsers
// http://www.quanttec.com/fparsec/users-guide/debugging-a-parser.html
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        #if DEBUG
        eprintfn "%A: Entering %s" stream.Position label
        #endif
        let reply = p stream
        #if DEBUG
        eprintfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        #endif
        reply

/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2 <!> "p2") |>> 
    function
    | a, Some(b) -> if2(a, b)
    | a, None -> a

let lineComment : Parser<_> = COMMENT >>. skipRestOfLine false

/// Parses a list of comma-separated elements (each element is parsed by pelem).
let listDef str pelem = 
    (ws (skipString str))
    >>. (ws EQ) >>. betweenBrackets (sepbycommas pelem)

///Parses a set of elements. Fails if two elements are equal according to
///mappingFn. 
let setDef str pelem mappingFn = 
    (listDef str pelem) 
    >>= (fun x -> fun _ -> 
        let dup = x |> List.map mappingFn |> List.duplicates
        if dup.Length > 0 then
            dup.ToString()
            |> sprintf "%s: multiple definitions of %s" str
            |> (fun msg -> Reply(Error, ErrorMessageList(Message(msg))))
        else
            Reply(x |> Set.ofList))

let unexpected msg = fun _ -> Reply(Error, ErrorMessageList(Unexpected(msg)))

let toMap lst =
    let dup = lst |> List.map fst |> List.duplicates
    if dup.Length > 0 then
        withcommas dup
        |> sprintf "Multiple definitions of %s"
        |> unexpected
    else
        preturn (lst |> Map.ofList)

/// Parses a single init definition.
let pinit loc =
    let pChoose = 
        (sepbycommas pint32) |> betweenBrackets |>> Choose
    let pRange = 
        followedBy (pint32 >>. (skipString ".."))
        >>. ((ws pint32) .>>. ((skipString "..") >>. (ws pint32)) 
        |>> Range)
    let pSingle = (ws pint32) |>> (Choose << List.singleton)

    let pvar = 
        pipe2
            KEYNAME
            (opt (betweenBrackets puint32))
            (fun name -> function
            | Some(b) -> {vartype=Array(int b); name=name; location=loc}
            | None -> {vartype=Scalar; name=name; location=loc})

    pvar .>>. ((ws COLON) >>. ws (choice [pChoose; pRange; pSingle]))

let pkeys loc = 
    (ws (sepbycommas (ws (pinit loc))) >>= toMap)

let pstringEq str p = 
    (ws (skipString str) >>. (ws EQ) >>. p)