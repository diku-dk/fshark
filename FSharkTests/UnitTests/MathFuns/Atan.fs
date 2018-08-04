module Atan
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let atann (x : single) (y : double) : (single * single * double * double) =
    (atan x, 
     atan <| - x, 
     atan y, 
     atan <| - y)

[<FSharkInput>]
let value = [|0.5f; 0.5|] : obj array

[<FSharkOutput>]
let sameValue = (0.46364f,-0.46364f ,0.46364,-0.46364) : (single * single * double * double)
