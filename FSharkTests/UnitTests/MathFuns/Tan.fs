module Sin
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let sinn (x : single) (y : double) : (single * single * double * double) =
    (sin x, 
     sin <| - x, 
     sin y, 
     sin <| - y)

[<FSharkInput>]
let value = [|45.0f; 45.0|] : obj array

[<FSharkOutput>]
let sameValue = (0.8509f, -0.8509f, 0.8509, -0.8509) : (single * single * double * double)
