namespace Frontend
open FSharpPlus
open FSharpPlus.Data
open Message

type Outcome<'a> = Result<'a * Message<Warn> list, Message<Warn> list * Message<Err> list>

module Outcome = 
    let transform f x =
        let fn y =
            try
                match f (fst y) with
                | Ok(state, warn) -> Outcome.Ok (state, Operators.plus (snd y) warn)
                | Error(warn, err) -> Error(Operators.plus (snd y) warn, err)
            with :? LabsException as e -> Error(snd y, [e.Data0])
        Result.bind fn x
        
    let check f x =
        x >>= (fun y -> transform f x >>= fun _ -> Ok y)
    
    let zero x = Outcome.Ok(x, []) 
    let inline wrap x warn err =
        if err = [] then Outcome.Ok(x, warn)
        else Error(warn, err)
    
    let inline (<?>) x f = check f x
    let inline (<??>) x f = x <?> fun _ -> f
    
    let inline (<~>) x f = transform f x
    let inline (<~~>) x f = x <~> fun _ -> f
    
    
    let traverse (fn: 'a -> Outcome<_>) lst = Result.traverse fn lst
    
    let fold fn lst state =
        Seq.fold (fun s x -> s <~> (fn x)) (zero state) lst
