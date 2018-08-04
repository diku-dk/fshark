module Max
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let maxx 
    (x8 : uint8) (y8 : uint8)
    (x : int) (y : int)
    (xdouble : double) (ydouble : double) : (uint8 * int * double) =
    (max x8 y8,
    max x y,
    max xdouble ydouble)
    

[<FSharkInput>]
let value = [|4uy; 127uy; 31; 32; (-infinity); 4.0|] : obj array

[<FSharkOutput>]
let sameValue = (127uy, 32, 4.0) : (uint8 * int * double)
