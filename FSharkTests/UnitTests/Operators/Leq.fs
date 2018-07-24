module Leq
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let leq (fiveByte : int8) (fiveShort : int16) (five : int) (fiveLong : int64) (fiveSingle : single) (fiveDouble : double) :  (bool * bool * bool * bool * bool * bool) =
    (fiveByte <= 2y, fiveShort <= 2s, five <= 2, fiveLong <= 2L, fiveSingle <= 2.0f, fiveDouble <= 2.0)

[<FSharkInput>]
let value = [|5y; 5s; 5; 5L; 5.0f; 5.0|] : obj array

[<FSharkOutput>]
let sameValue = (false, false, false, false, false, false) : (bool * bool * bool * bool * bool * bool)


