module Log
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let logg (x : single) (y : double) : (single * single * double * double) =
    (log x, 
     log <| - x, 
     log y, 
     log <| - y)

[<FSharkInput>]
let value = [|7.5f; 7.5|] : obj array

[<FSharkOutput>]
let sameValue = (2.0149f, nanf, 2.0149, nan) : (single * single * double * double)
