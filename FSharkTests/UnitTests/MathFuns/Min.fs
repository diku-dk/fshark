module Min
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let minn 
    (x8 : uint8) (y8 : uint8)
    (x : int) (y : int)
    (xdouble : double) (ydouble : double) : (uint8 * int * double) =
    (min x8 y8,
     min x y,
     min xdouble ydouble)
    

[<FSharkInput>]
let value = [|4uy; 128uy; 31; 32; (-infinity); 4.0|] : obj array

[<FSharkOutput>]
let sameValue = (4uy, 31, -infinity) : (uint8 * int * double)
