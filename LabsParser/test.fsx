#I "/Users/luca/.nuget/packages/fparsec/1.0.1/lib/net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "bin/Debug/netstandard2.0/LabsCore.dll"
#r "bin/Debug/netstandard2.0/LabsParser.dll"

open FParsec

let run = FParsec.CharParsers.run

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


run Parser.full example
;;