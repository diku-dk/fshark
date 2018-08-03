module Floor
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let florr (x : single) (y : double) : (single * single * double * double) =
    (floor x, 
     floor <| - x, 
     floor y, 
     floor <| - y)

[<FSharkInput>]
let value = [|3.14159f; 3.14159|] : obj array

[<FSharkOutput>]
let sameValue = (3.0f, -4.0f, 3.0, -4.0) : (single * single * double * double)
