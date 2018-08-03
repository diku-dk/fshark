module Round
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let roundd (x : single) (y : double) : (single * single * double * double) =
    (round x, 
     round <| - x, 
     round y, 
     round <| - y)

[<FSharkInput>]
let value = [|4.5f; 5.5|] : obj array

[<FSharkOutput>]
let sameValue = (4.0f, -4.0f, 6.0, -6.0) : (single * single * double * double)
