module Sinh
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
let value = [|1.0f; 1.0|] : obj array

[<FSharkOutput>]
let sameValue = (1.1752f, -1.1752f, 1.1752, -1.1752) : (single * single * double * double)
