module Tanh
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let tanhh (x : single) (y : double) : (single * single * double * double) =
    (tanh x, 
     tanh <| - x, 
     tanh y, 
     tanh <| - y)

[<FSharkInput>]
let value = [|1.0f; 1.0|] : obj array

[<FSharkOutput>]
let sameValue = (0.76159f, -0.76159f, 0.76159, -0.76159) : (single * single * double * double)
