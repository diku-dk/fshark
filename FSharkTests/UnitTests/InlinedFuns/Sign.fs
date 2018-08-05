module Sign
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let signn (fourByte : int8) (fourShort : int16) (zero : int) (fourLong : int64) (fourSingle : single) (fourDouble : double) : (int * int* int * int* int * int) =
    (sign fourByte, 
     sign <| - fourShort, 
     sign zero, 
     sign <| - fourLong, 
     sign  fourSingle, 
     sign <| - fourDouble)

[<FSharkInput>]
let value = [|4y; 4s; 0; 4L; 4.0f; 4.0|] : obj array

[<FSharkOutput>]
let sameValue = (1,-1,0,-1,1,-1) : (int * int * int * int * int * int)
