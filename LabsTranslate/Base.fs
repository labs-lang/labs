module internal Base 
open FParsec
open FSharpPlus.Operators
open LabsCore
open Parser

type KeyMapping = Map<string, int>

// An entrypoint contains:
// * A mapping from pcs to their required value
// * A set of "sibling" pcs (when inside a parallel process)
// * The process' own pc
type EntryPoint<'a when 'a : comparison> =
    Map<Base<'a, Position>, Map<int, int> * Set<int> * int>
type EntryPointInfo<'a when 'a : comparison> = {
    mypc : int
    maxpc : int
    pcs : Map<int, int>
    entrypoints : EntryPoint<'a>
    par : Set<int>
}
with 
    member this.increment =
        let newpc = this.pcs.TryFind this.mypc |> Option.defaultValue 0 |> (+) 1 
        {this with pcs = Map.add this.mypc newpc this.pcs}
    member this.spawnpc n = {
        this with 
            mypc = this.mypc + n
            maxpc = this.mypc + n
            pcs = Map.add (this.mypc + n) 0 this.pcs
    }
    static member init : EntryPointInfo<'a> = {
        mypc = 0
        maxpc = 0
        pcs = [(0, 0)] |> Map.ofList
        entrypoints = Map.empty
        par = Set.empty
    }

let joinEntrypoints s =
    Seq.fold (
        Map.fold (fun (st:Map<_,_>) k v ->
            match st.TryFind k with
            | Some oldset -> Map.add k (Set.add v oldset) st
            | None -> Map.add k (Set.singleton v) st
        )    
    ) Map.empty s

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

let chStreamName name b =
    let (p: Position)=b.pos in let newPos = Position(name, p.Index, p.Line, p.Column)
    {b with pos = newPos}

// Keep track of the unfolded process
let chpos name pos (b:Base<_,Position>) =
    chStreamName (sprintf "%s@%O" name pos) b