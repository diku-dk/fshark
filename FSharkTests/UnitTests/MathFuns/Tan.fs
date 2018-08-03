module Tan
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let tann (x : single) (y : double) : (single * single * double * double) =
    (tan x, 
     tan <| - x, 
     tan y, 
     tan <| - y)

[<FSharkInput>]
let value = [|45.0f; 45.0|] : obj array

[<FSharkOutput>]
let sameValue = (1.61977f, -1.61977f, 1.61977, -1.61977) : (single * single * double * double)
