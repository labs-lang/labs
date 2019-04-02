// Learn more about F# at http://fsharp.org

open System
open System.IO
open FParsec
open FSharpPlus
open FSharpPlus.Lens
//open FSharpPlus.Operators


open Checker
open Checker
open Checker
open Checker
open Checker.Types
open Types
open Parser
open Duplicate
open Undefined


let wrapParserResult p text =
        try
            let x = CharParsers.run p text
            match x with
            | Success(a, _, _) -> a
            | Failure(errorMsg, _, _) -> 
                failwithf "Parsing failed:\n %s" errorMsg
        with
            ex -> failwith ex.Message




let pprint = function | Result.Ok _ -> "Ok" | Result.Error x -> sprintf "ERR %O" x

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText argv.[0]
    let ast = wrapParserResult Parser.parse input
    
    eprintfn(">>> Begin AST check")
     
    zero (SymbolTable.empty "top" (Position("",int64 0,int64 0,int64 0)) Top)
//    <~> tryAddVar {name=""; position=Position("",int64 0,int64 0,int64 0); location=E; vartype=Scalar; init=Choose [Leaf(Const(1))]}
    <~> trFold tryAddVar (ast^._1).environment
    <~> trFold mapVar (ast^._1).environment
    <~> trFold tryAddAgent (ast^._3)
    <~> trFold tryAddStigmergy (ast^._2)
    <?> Undefined.checkProcessNames 
    
    |> eprintfn "%A\n END AST CHECK"
//    |> printfn "%s"
    0 // return an integer exit code
