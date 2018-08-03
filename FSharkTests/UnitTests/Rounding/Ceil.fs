module Ceil
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let ceill (x : single) (y : double) : (single * single * double * double) =
    (ceil x, 
     ceil <| - x, 
     ceil y, 
     ceil <| - y)

[<FSharkInput>]
let value = [|3.14159f; 3.14159|] : obj array

[<FSharkOutput>]
let sameValue = (4.0f, -2.0f, 4.0, -2.0) : (single * single * double * double)
