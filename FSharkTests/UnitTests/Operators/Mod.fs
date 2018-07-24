module Mod
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let modd (fiveByte : int8) (fiveShort : int16) (five : int) (fiveLong : int64) : (int8 * int16 * int * int64) =
    (fiveByte % 2y, fiveShort % 2s, five % 2, fiveLong % 2L)

[<FSharkInput>]
let value = [|5y; 5s; 5; 5L|] : obj array

[<FSharkOutput>]
let sameValue = (1y, 1s, 1, 1L) : (int8 * int16 * int * int64)


