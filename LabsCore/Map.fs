module Map
// https://github.com/fsprojects/FSharpPlus/blob/master/src/FSharpPlus/Extensions.fs
// See LICENSE_FSharpPlus.txt

/// Returns the set of keys in source.
let keys   (source: Map<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
/// Returns the set of keys in source.
let values (source: Map<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

/// <summary>Map values of the original Map.</summary>
/// <remarks>Keys remain unchanged.</remarks>
/// <param name="f">The mapping function.</param>
/// <param name="x">The input Map.</param>
///
/// <returns>The mapped Map.</returns>
let mapValues f (x: Map<'Key, 'T>) = Map.map (fun _ -> f) x

/// Returns the union of two maps, using the combiner function for duplicate keys.
let unionWith combiner (source1: Map<'Key, 'Value>) (source2: Map<'Key, 'Value>) =
    Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> combiner k v v' | None -> v') m) source1 source2

/// Returns the union of two maps, preferring values from the first in case of duplicate keys.
let union (source: Map<'Key, 'T>) (altSource: Map<'Key, 'T>) = unionWith (fun _ x _ -> x) source altSource
