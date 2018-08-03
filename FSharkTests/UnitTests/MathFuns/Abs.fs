module Abs
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let abss (fourByte : int8) (fourShort : int16) (four : int) (fourLong : int64) (fourSingle : single) (fourDouble : double) : (int8 * int16 * int * int64 * single * double) =
    (abs <| - fourByte, 
     abs <| - fourShort, 
     abs <| - four, 
     abs <| - fourLong, 
     abs <| - fourSingle, 
     abs <| - fourDouble)

[<FSharkInput>]
let value = [|4y; 4s; 4; 4L; 4.0f; 4.0|] : obj array

[<FSharkOutput>]
let sameValue = (4y, 4s, 4, 4L, 4.0f, 4.0) : (int8 * int16 * int * int64 * single * double)
