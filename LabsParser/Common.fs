[<AutoOpen>]
module internal Common
open FParsec

type Parser<'t> = Parser<'t, unit>

// Tokens
//-----------------------------------------
let NEG : Parser<_> =           (skipChar '!')
let CONJ : Parser<_> =          (skipChar '&')
let EQ : Parser<_> =            (skipChar '=')
let COMMENT : Parser<_> =       (skipChar '#')
//-----------------------------------------

let isAlphanum x = isAsciiLetter x || isDigit x
let isAlphanumlower x = isAsciiLower x || isDigit x

let KEYNAME : Parser<_> =
    asciiLower 
    .>>. (manySatisfy isAlphanumlower)
    |>> (fun (x, y) -> (string x) + y)

let IDENTIFIER : Parser<_> = 
    asciiUpper 
    .>>. (manySatisfy isAlphanum)
    |>> (fun (x, y) -> (string x) + y)


let withcommas x = (String.concat ", " x)


/// Parse p and skip whitespace after.
let ws p = p .>> spaces
let betweenBrackets p = between (skipChar '[') (skipChar ']') p
let betweenBraces p = between (skipChar '{') (skipChar '}') p
let betweenParen p = between (skipChar '(') (skipChar ')') p
let sepbycommas p = sepBy p (ws (skipChar ','))

/// Apply parser p1, then apply optional parser p2.
/// If p2 succeeds, pass both results to if2.
/// Otherwise return the result of p1.
let maybeTuple2 p1 p2 if2 =
    p1 .>>. (opt p2) |>> function
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
        else Reply(x |> Set.ofList))

let unexpected msg = fun _ -> Reply(Error, ErrorMessageList(Unexpected(msg)))

let toMap lst =
    let dup = lst |> List.map fst |> List.duplicates
    if dup.Length > 0 then
        withcommas dup
        |> sprintf "Multiple definitions of %s"
        |> unexpected
    else preturn (lst |> Map.ofList)


/// Parses a point (i.e. a pair of integers)
let ppoint : Parser<_> =
    (betweenParen (pint32 .>>. (ws (skipChar ',') >>. pint32)))


/// Parses a single init definition.
let pinit =
    let pinitI =
        followedBy (pint32 >>. (skipChar ':'))
        >>. ((ws pint32)
        .>>. ((skipChar ':') >>. (ws pint32)) 
        |>> RangeI)
        <|> (sepbycommas pint32 |>> ChooseI)

    let pinitP =
        followedBy (ppoint >>. (skipChar ':'))
        >>. ((ws (ppoint))
        .>>. ((skipChar ':') >>. (ws ppoint))
        |>> RangeP)
        <|> (sepbycommas ppoint |>> ChooseP)

    (ws KEYNAME) .>>. betweenBraces (choice [followedBy pint32 >>. pinitI; pinitP])

