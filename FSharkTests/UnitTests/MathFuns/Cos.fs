module Cos
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let coss (x : single) (y : double) : (single * single * double * double) =
    (cos x, 
     cos <| - x, 
     cos y, 
     cos <| - y)

[<FSharkInput>]
let value = [|3.14159f; 3.14159|] : obj array

[<FSharkOutput>]
let sameValue = (-1.000f, -1.000f, -1.000, -1.000) : (single * single * double * double)
