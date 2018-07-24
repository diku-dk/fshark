module Eq
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let eq (fiveByte : int8) (fiveShort : int16) (five : int) (fiveLong : int64) (fiveSingle : single) (fiveDouble : double) (b : bool): (int8 * int16 * int * int64 * single * double * bool) =
    (fiveByte = 2y, fiveShort = 2s, five = 2, fiveLong = 2L, fiveSingle = 2.0f, fiveDouble = 2.0, b = true)

[<FSharkInput>]
let value = [|5y; 5s; 5; 5L; 5.0f; 5.0; false|] : obj array

[<FSharkOutput>]
let sameValue = (false, false, false, false, false, false, false) : (bool * bool * bool * bool * bool * bool * bool)

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
