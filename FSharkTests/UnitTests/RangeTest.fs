module RangeTest
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (n : int) : int array = [|1..5|]
[<FSharkInput>]
let value = [|0|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3;4;5|] : int array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
