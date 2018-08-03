module Acos
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let acoss (x : single) (y : double) : (single * single * double * double) =
    (acos x, 
     acos <| - x, 
     acos y, 
     acos <| - y)

[<FSharkInput>]
let value = [|0.5f; -0.5|] : obj array

[<FSharkOutput>]
let sameValue = (1.0471f, 2.1176f, 1.0471, 2.1176) : (single * single * double * double)
