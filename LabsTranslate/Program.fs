open Base
open Types
open Checks 
open Encode
open Templates
open ArgParse
open EncodeInit
open System.IO
open Result
open FSharpPlus.Operators
open LabsCore

[<EntryPoint>]
let main argv =

    let checks sys = 
        async {
            let! t1 = async { return checkNames sys } |> Async.StartChild
            let! t2 = async { return checkComponents sys } |> Async.StartChild
            let! t3 = async { return analyzeKeys sys } |> Async.StartChild
            let! r1 = t1
            let! r2 = t2
            let! r3 = t3

            return (r1 >>= (fun _ -> r2) >>= fun _ -> r3)
        }
        |> Async.RunSynchronously


    let readAndCheck (cli:Argu.ParseResults<_>) =
        File.ReadAllText (cli.GetResult Arguments.File)
        |> fun x -> Base.parse x (placeholders cli)
        >>= checks
        |> logErr

    let doTranslate (cli:Argu.ParseResults<_>) (s, m, maxI, maxL, maxE) =
        let toVar =
            let tmp = toVarSystem s m
            // Remove agents that have not been spawned
            {tmp with components= Map.filter (fun name _ -> tmp.spawn.ContainsKey name) tmp.components }
        let mergeProcesses (agent:ComponentDef<_>) = Map.union agent.processes toVar.processes
        let s' =
            let unfoldedBehavior (agent:ComponentDef<_>) =
                let b =
                    Process.expand chpos (mergeProcesses agent) "Behavior"
                    |> Process.simplify
                {agent with processes = Map.add "Behavior" b agent.processes}
            {toVar with components = Map.mapValues unfoldedBehavior toVar.components}

        let bound = cli.GetResult (Bound, defaultValue=1)
//            let sys, trees, _, _, _, _ = x
//            [
//                translateHeader (cli.Contains Simulation) (cli.Contains No_Bitvector)  bound x
//                translateInit (sys, trees, m)
//                translateAll trees
//                translateCanProceed trees
//                translateMain (cli.Contains Fair) (sys, trees)
//            ]
//            |> List.map ((Result.map (printfn "%s")) >> logErr)
//            |> List.reduce (<&&>)

            
        let setEntryAgent info _ (agent:ComponentDef<_>) =
            setentry info agent.processes.["Behavior"]
        
        let mapAgents f =
            Map.mapValues (fun (agent:ComponentDef<_>) ->
                (f agent) agent.processes.["Behavior"])
        
        let entrypoints =
            Map.fold setEntryAgent EntryPointInfo.init s'.components
            |> fun x -> x.entrypoints
        let guards =
            mapAgents (fun _ -> setGuards) s'.components
            |> Map.fold (fun state _ m -> Map.union state m) Map.empty
        let exit = mapAgents (fun v -> setExit entrypoints (mergeProcesses v)) s'.components
    
        let translateAgents = 
            mapAgents (fun v -> encode entrypoints exit.[v.name] guards) s'.components
            |> Map.values
        // Allons-y!    
        [
            translateInit s' entrypoints m
            translateHeader (cli.Contains Simulation) (cli.Contains No_Bitvector) bound s' m entrypoints (maxI, maxL, maxE)
        ]
        |> Seq.append translateAgents
        |> Seq.append [translateMain (cli.Contains Fair) s' entrypoints guards]
        |> Seq.rev
        |> Seq.map ((Result.map (printfn "%s")) >> logErr)
        |> Seq.reduce (fun r1 r2 -> r1 >>= fun _ -> r2)
        |> setReturnCode

    let doInfo (s,m,_,_,_) = 
        serializeInfo (s, m)
        |> logErr // Log any error at the end
        |> setReturnCode

    try
        let parsedCli = parseCLI argv
        readAndCheck parsedCli
        |>> (fun x ->
            if parsedCli.Contains Info
            then doInfo x
            else doTranslate parsedCli x)
        |> 
            function | Ok i -> i
                     | Error _ -> 10
    with 
        e -> 
            eprintfn "%s" e.Message
            #if DEBUG
            eprintfn "%s" e.StackTrace
            #endif
            10