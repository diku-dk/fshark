module Asin
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let asinn (x : single) (y : double) : (single * single * double * double) =
    (asin x, 
     asin <| - x, 
     asin y, 
     asin <| - y)

[<FSharkInput>]
let value = [|0.5f; 0.5|] : obj array

[<FSharkOutput>]
let sameValue = (0.52359f,-0.52359f,0.52359,-0.52359) : (single * single * double * double)
