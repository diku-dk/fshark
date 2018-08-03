module Exp
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let expp (x : single) (y : double) : (single * single * double * double) =
    (exp x, 
     exp <| - x, 
     exp y, 
     exp <| - y)

[<FSharkInput>]
let value = [|3.14159f; 3.14159|] : obj array

[<FSharkOutput>]
let sameValue = (23.140f, 0.04321f, 23.140, 0.04321) : (single * single * double * double)
