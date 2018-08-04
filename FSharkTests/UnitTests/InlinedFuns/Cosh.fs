module Cosh
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let cossh (x : single) (y : double) : (single * single * double * double) =
    (cosh x, 
     cosh <| (-x), 
     cosh y, 
     cosh <| (-y)
    )
    
[<FSharkInput>]
let value = [|1.0f; 1.0|] : obj array

[<FSharkOutput>]
let sameValue = (1.543f, 1.543f, 1.543, 1.543) : (single * single * double * double)
