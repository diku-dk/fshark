module Test
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let identity (x : int) : int = x

[<FSharkInput>]
let value = [|4|] : obj array

[<FSharkOutput>]
let sameValue = 4 : int

