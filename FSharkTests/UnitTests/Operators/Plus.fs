module Plus
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let plus (fourByte : int8) (fourShort : int16) (four : int) (fourLong : int64) (fourSingle : single) (fourDouble : double) : (int8 * int16 * int * int64 * single * double) =
    (fourByte + 3y, fourShort + 3s, four + 3, fourLong + 3L, fourSingle + 3.0f, fourDouble + 3.0)

[<FSharkInput>]
let value = [|4y; 4s; 4; 4L; 4.0f; 4.0|] : obj array

[<FSharkOutput>]
let sameValue = (7y, 7s, 7, 7L, 7.0f, 7.0) : (int8 * int16 * int * int64 * single * double)

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
