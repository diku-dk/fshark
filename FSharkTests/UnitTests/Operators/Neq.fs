module Neq
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let neq (fiveByte : int8) (fiveShort : int16) (five : int) (fiveLong : int64) (fiveSingle : single) (fiveDouble : double) (b : bool):  (bool * bool * bool * bool * bool * bool * bool) =
    (fiveByte <> 2y, fiveShort <> 2s, five <> 2, fiveLong <> 2L, fiveSingle <> 2.0f, fiveDouble <> 2.0, b <> true)

[<FSharkInput>]
let value = [|5y; 5s; 5; 5L; 5.0f; 5.0; false|] : obj array

[<FSharkOutput>]
let sameValue = (true, true, true, true, true, true, true) : (bool * bool * bool * bool * bool * bool * bool)


