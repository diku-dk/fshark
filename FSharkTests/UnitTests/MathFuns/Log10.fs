module Log10
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let logg10 (x : single) (y : double) : (single * single * double * double) =
    (log10 x, 
     log10 <| - x, 
     log10 y, 
     log10 <| - y)

[<FSharkInput>]
let value = [|200.0f; 200.0|] : obj array

[<FSharkOutput>]
let sameValue = (2.301f, nanf, 2.301, nan) : (single * single * double * double)
