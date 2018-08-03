module Sqrt
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let sqrt (a : single) (b : single) (c : double) (d : double) : (single * single * double * double) = id v
 
[<FSharkInput>]
let value = [|4.0f; 50.0f; 4.0; 50.0 |] : obj array

[<FSharkOutput>]
let sameValue = (2.0f, 7.0710f, 2.0, 7.0710) : (single * single * double * double)

