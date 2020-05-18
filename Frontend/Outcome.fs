namespace Frontend
open FSharpPlus
open FSharpPlus.Data
open Message

/// Helper type. An Outcome may be successful, 
/// with a value of 'a and a list of warnings, or a failure,
/// with a list of warnings and errors.
type Outcome<'a> = Result<'a * Message<Warn> list, Message<Warn> list * Message<Err> list>

module Outcome = 
    /// <summary>
    /// Applies function <c>f</c> to the content of <c>x</c>.
    /// </summary>
    /// <remark>
    /// <c>Outcome.transform</c> is essentially a wrapper around
    /// <c>Result.bind</c>.
    /// </remark>
    /// <param name="f">A function from the type <c>'a</c> of the input outcome
    /// to an outcome of type <c>'b</c>.</param>
    /// <param name="x">The input outcome.</param>
    /// <returns>An outcome of type <c>'b</c>, accumulating the warnings of <c>x</c>.</returns>
    let transform f x =
        let fn y =
            try
                match f (fst y) with
                | Ok (state, warn) -> Outcome.Ok (state, plus (snd y) warn)
                | Error (warn, err) -> Error (plus (snd y) warn, err)
            with :? LabsException as e -> Error (snd y, [e.Data0])
        Result.bind fn x

    
    /// <summary>
    /// Like <c>transform</c>, but if the outcome of <c>f</c> is successful its
    /// value is discarded and the value of <c>x</c> is propagated instead.
    /// </summary>
    /// <seealso cref="transform"/>
    /// <returns></returns>
    let check f x =
        x >>= (fun y -> transform f x >>= fun _ -> Ok y)
    
    /// Returns an Ok outcome with no warnings and result x.
    let zero x = Outcome.Ok(x, []) 
    
    /// Wraps result <c>x</c>, warnings <c>warn</c> and errors <c>err</c> 
    /// into the correct <c>Outcome</c> type.
    let inline wrap x warn err =
        if List.isEmpty err then Outcome.Ok(x, warn)
        else Error(warn, err)

    /// <summary>
    /// f &lt;?&gt; x is equivalent to <c>check f x</c>.
    /// </summary>
    /// <seealso cref="check"/>
    let inline (<?>) x f = check f x
    
    /// If x is Ok, it performs a check with function f.
    let inline (<??>) x f = x <?> fun _ -> f
    
    /// <summary>
    /// f &lt;~&gt; x is equivalent to <c>transform f x</c>.
    /// </summary>
    /// <seealso cref="check"/>
    let inline (<~>) x f = transform f x
    
    /// If x is Ok, it performs a transform with function f().
    let inline (<~~>) x f = x <~> fun _ -> f
    
    /// Implements traverse on the Outcome type.
    // let traverse (fn: 'a -> Outcome<_>) lst = Result.traverse fn lst
    
    /// <summary>
    /// Applies a function to each element of the collection, threading an
    /// outcome as the accumulator of the computation.
    /// If the input function is <c>f</c> and the elements of the collection are
    /// <c>i0...,iN<c> then <c>fold</c> computes
    /// (zero state) <~> f i0 <~> ... <~> f iN.
    /// </summary>
    /// <param name="f">A function that updates the outcome with each element
    /// from the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <param name="state">The initial outcome,</param>
    /// <typeparam name="'a"></typeparam>
    /// <typeparam name="'b"></typeparam>
    /// <returns></returns>
    let fold f source state =
        Seq.fold (fun s x -> s <~> (f x)) (zero state) source
