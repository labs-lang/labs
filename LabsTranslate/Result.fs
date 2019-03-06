module Result

let foldErrors joinfn acc source = 
    let fn a = function | Ok _ -> a | Error e -> (joinfn a e)
    Seq.fold fn acc source

let setReturnCode r =
    match r with 
    | Ok _ -> 0
    | Error _ -> 10

let log msg = function 
    | Ok a -> 
        eprintfn "\n%s" msg
        Ok a
    | Error e -> Error e

let logErr = function
    | Ok a -> Ok a
    | Error s -> 
        eprintfn "[Error] %s" s
        Error ""