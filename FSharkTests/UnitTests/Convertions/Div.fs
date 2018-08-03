module Div
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let div (fiveByte : int8) (fiveShort : int16) (five : int) (fiveLong : int64) (fiveSingle : single) (fiveDouble : double) : (int8 * int16 * int * int64 * single * double) =
    (fiveByte / 2y, fiveShort / 2s, five / 2, fiveLong / 2L, fiveSingle / 2.0f, fiveDouble / 2.0)

[<FSharkInput>]
let value = [|5y; 5s; 5; 5L; 5.0f; 5.0|] : obj array

[<FSharkOutput>]
let sameValue = (2y, 2s, 2, 2L, 2.5f, 2.5) : (int8 * int16 * int * int64 * single * double)

