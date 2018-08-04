module TupleNaming
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let tuple (x:int) = (1, 2,5) : (int * int * int)

[<FSharkInput>]
let value = [|0|] : obj array

[<FSharkOutput>]
let sameValue = (1, 2, 5) : (int * int * int)

