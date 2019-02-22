#I "/Users/luca/.nuget/packages/fparsec/1.0.1/lib/net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "bin/Debug/netstandard2.0/LabsCore.dll"
#r "bin/Debug/netstandard2.0/LabsParser.dll"

open FParsec
open System.IO

let run = FParsec.CharParsers.run

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

//let example = File.ReadAllText "/Users/luca/Projects/labs/examples/flock.labs"
//run Parser.pre example
//let strip = run Parser.stripComments example
//match strip with
//| Success (result, _, _) -> run Parser.parse result
//| _ -> failwith "aaa"

;;