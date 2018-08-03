module Trunc
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let truncc (x : single) (y : double) : (single * single * double * double) =
    (truncate x, 
     truncate <| - x, 
     truncate y, 
     truncate <| - y)

[<FSharkInput>]
let value = [|3.14159f; 3.14159|] : obj array

[<FSharkOutput>]
let sameValue = (3.0f, -3.0f, 3.0, -3.0) : (single * single * double * double)
