module internal Base 
open FParsec
open FSharpPlus.Operators
open LabsCore
open Parser

type KeyMapping = Map<string, int>

let withcommas x = (String.concat ", " x)

let parse text (placeholders:Map<string, string>) =
    let checkPlaceholders s =
        (Map.keys placeholders)
        |> Set.ofSeq
        |> Set.difference s
        |> fun z -> 
            if (Set.isEmpty z)
            then Result.Ok s 
            else Result.Error(sprintf "Uninitialized external variable: %s" (withcommas z))

    let wrapParserResult p text = 
        try
            let x = CharParsers.run p text
            match x with
            | Success(a, _, _) -> Result.Ok a
            | Failure(errorMsg, _, _) -> 
                Result.Error (sprintf "Parsing failed:\n %s" errorMsg)
        with
            ex -> Result.Error ex.Message

    let stripped =
        CharParsers.run stripComments text
        |> function | Success(a, _, _) -> a | Failure(msg,_,_) -> failwith msg

    let defPlaceholders =    
        stripped
        |> (wrapParserResult pre)
        |>> (Set.filter ((<>) ""))
        >>= checkPlaceholders
        >>= fun def ->
            // Check for undefined external variables
            wrapParserResult allPlaceholders stripped
            |>> Set.ofList |>> (Set.filter ((<>) ""))
            |>> fun all -> (Set.difference all def)
            >>= (fun diff ->
                if Set.isEmpty diff then Result.Ok def else 
                diff
                |> Set.map ((+) "_")
                |> withcommas
                |> sprintf "External variables %s have not been defined in the 'extern' section." 
                |> Result.Error)

    defPlaceholders
    |>> ((Set.fold (fun (txt:string) ph -> txt.Replace("_"+ph, placeholders.[ph])) stripped))
    >>= (wrapParserResult Parser.parse)

let enumerate s = 
    s
    |> Seq.mapi (fun i x -> x, i)
    |> Map.ofSeq

let makeCounter (start: int) =
    let x = ref start
    let incr() =
        x := !x + 1
        !x
    incr

