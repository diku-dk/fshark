module Pathfinder
open FSharkPrelude
// Code and comments based on
// https://github.com/kkushagra/rodinia/blob/master/openmp/pathfinder/
//
// ==
//
// input @ data/medium.in
// output @ data/medium.out

let in_range (x: int) (lb: int) (ub: int): bool = (x >= lb) && (x <= ub)
let clamp_range (x: int) (lb: int) (ub: int): int =
    if (x < lb) then lb
    else if (x > ub) then ub else x

let min (a: int) (b: int): int = if (a <= b) then a else b


//----------------------------------------
// Util: Sobol random number generation --
//----------------------------------------
let sobolDirVcts : int [] =
    [| 536870912; 268435456; 134217728; 67108864; 33554432; 16777216; 8388608; 4194304; 2097152; 1048576;
       524288;    262144;    131072;    65536;    32768;    16384;    8192;    4096;    2048;    1024;
       512;       256;       128;       64;       32;       16;       8;       4;       2;       1      |]

let sobolInd (dirVct : int []) (n : int) : int =
    let n_gray = (n >>> 1) ^^^ n
    let res = 0
    in Foldr (fun i res' ->
        let t = 1 <<< i
        in if (n_gray &&& t) = t
           then res' ^^^ dirVct.[i]
           else res'
    ) res (Iota 30)

[<FSharkEntry>]
let main (cols: int) (rows: int) (wall_flat : int array) : int array =
    let wall   = Unflatten rows cols wall_flat
    let result = Copy (wall.[0])

    //-------------
    // 1. Kernel --
    //-------------
    in Foldr (fun t result' ->
        Map (fun (i: int) ->
                let res = result'.[i]
                let res = if (i > 0) then min res result'.[i-1] else res
                let res = if (i < cols-1) then min res result'.[i+1] else res
                in wall.[t+1].[i] + res
           ) (Iota cols)
       ) result (Iota (rows - 1))

