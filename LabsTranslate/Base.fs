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

let unfold (procs: Map<_, _>) name =
    // Keep track of the unfolded process
    let chpos name pos (b:Base<_,FParsec.Position>) =
        let p = b.pos in let newPos = Position(sprintf "%s@%O" name pos, p.Index, p.Line, p.Column)
        BaseProcess {b with pos = newPos}
        
    let rec unfold_ visited name = 
        let base_ b = 
            match b.stmt with
            | Name n when n=name || n="Behavior" -> BaseProcess b
            | Name n when (not (Set.contains b visited)) -> 
                unfold_ (visited.Add b) n 
                |> Process.map (chpos n b.pos) id
            | _ -> BaseProcess b
        Process.map base_ id procs.[name]
    unfold_ Set.empty name