module Templates
open Types
open Base

let updateKq keys = 
    (keys
    |> Seq.map (fun x -> sprintf "    setHin(tid, %i);" x)
    |> String.concat "\n") + "\n"

let brackets = sprintf "{\n%s}\n"

let assume = sprintf "    __VERIFIER_assume(%s);\n"

let entrypoint pc entry =
    assume <| (sprintf "pc[tid][%i] == %i" pc entry)

let exitpoint =
    sprintf "    pc[tid][%i] = %i;\n"


let signature = sprintf "%s_%i%s"

let attr = 
    sprintf """    int val = %s;
    attr(tid, %i, val, -1);
"""

let cfunc retType fName args body =
    (sprintf "%s %s (%s) {\n%s}\n") retType fName args body

let cvoid = cfunc "void"

let join name leftpc rightpc parentpc parententry parentexit = 
    cvoid (sprintf "%s_%i_Join" name parententry) "int tid"
    // Notice that, due to the structure of the encoding, pc=1 denotes termination
        ((entrypoint parentpc parententry) +
        (entrypoint leftpc 1) +
        (entrypoint rightpc 1) +
        (exitpoint leftpc 0) +
        (exitpoint rightpc 0) +
        (exitpoint parentpc parentexit)
        )


/// Encodes the skip process.
let skip pc next = 
    sprintf """
void stmt_nop%i (int tid) {
    __VERIFIER_assume(comp[tid].pc == %i);
    comp[tid].pc == %i;
}""" pc pc next

/// Encodes the stop process
let stop pc = 
    sprintf """
void stmt_nop%i (int tid) {
    __VERIFIER_assume(comp[tid].pc == %i);
    __VERIFIER_assume(0);
}""" pc pc

let globals = sprintf """
component comp[MAXPROCS];
int pc[MAXPROCS][%i];
int currenttimestamp;
"""

let forLoop = sprintf """
for (i=%i; i<%i; i++) {
    %s
}
""" 


let init arrayname key values =
    let guess = sprintf "guess%i" key

    let typeofVar =
        function
        | (a,b) when a >= 0     && b < 256      -> "unsigned char"
        | (a,b) when a > -128   && b < 128      -> "char"
        | (a,b) when a >= 0     && b < 65536    -> "unsigned short"
        | (a,b) when a > -32768 && b < 32768    -> "short"
        | (a,b) when a >=0                      -> "unsigned int"
        | _ -> "int"

    let nameI = sprintf " guess%i;\n" key
    let nameP = sprintf " guess%ix, guess%iy;\n" key key

    let assumeInt = sprintf "(%s == %i)" guess
    let assumeP = sprintf "(%sx == %i && %sy == %i)" 
    let assumeIntRange key minI maxI =
        sprintf "%s >= %i && %s <= %i" guess minI guess maxI
        |> assume
    let assumePRange key (xmin, ymin) (xmax, ymax)= 
        (sprintf "(%sx >= %i && %sx <= %i) && (%sy >= %i && %sy <= %i)"
        guess xmin guess xmax guess ymin guess ymax)
        |> assume

    let def = 
        function
        | ChooseI(l) -> (typeofVar (Seq.min l, List.max l) ) + nameI
        | ChooseP(l) ->
            let xs = l |> Seq.map fst
            let ys = l |> Seq.map snd
            let minval = min (Seq.min xs) (Seq.min ys)
            let maxval = max (Seq.max xs) (Seq.max ys)
            (typeofVar (minval, maxval) + nameP)
        | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + nameI)
        | RangeP((xmin, ymin), (xmax,ymax)) -> 
            let minval = min xmin ymin
            let maxval = max xmax ymax
            (typeofVar (minval, maxval) + nameP)
    let assign values =
        let prefix = 
            match values with
            | ChooseP(_)
            | RangeP(_) -> 
                (sprintf " int %s = packTuple(%sx, %sy);" guess guess guess)
            | _ -> ""
        "\n" + prefix + (sprintf "\n %s[%i] = %s;" arrayname key guess)

    (def values) + "\n" + (values
    |> (function
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " | "
            |> assume
        | ChooseP(ps) -> 
            ps 
            |> Seq.map (fun (x,y) ->(assumeP guess x guess y))
            |> String.concat "|"
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange key minI maxI
        | RangeP(minP, maxP) -> assumePRange key minP maxP
       )
    ) + (assign values) 