// Learn more about F# at http://fsharp.org

open System.IO
open FParsec
open StepResult
open Checks

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    // return an integer exit code

    File.ReadAllText("../examples/test.labs")
    |> log parse
    >>= log uniqueDefs
    >>= log checkNames
    >>= log checkInit
    |> ignore


    0