module Atan2
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let atann2 (x1 : single) (x2 : single) (y1 : double) (y2 : double) : (single * single * double * double) =
    (atan2 x1 x2,
     atan2 (-x1) (-x2), 
     atan2 (y1) (y2), 
     atan2 (-y1) (-y2))

[<FSharkInput>]
let value = [|0.5f; 0.7f; 0.5; 0.7|] : obj array

[<FSharkOutput>]
let sameValue = (0.620f, -2.52134f, 0.620, -2.52134): (single * single * double * double)
