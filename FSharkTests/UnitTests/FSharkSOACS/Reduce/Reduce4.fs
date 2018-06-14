module Reduce4
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>] // let m be the length of the array arrays, in this case 2
let zip1a (xs : int array array) (m : int) : int array = Reduce_Comm (fun acc r -> Map2 (+) acc r) (Replicate m 0) xs

[<FSharkInput>]
let value = [|[|[|1;2|];[|3;4|];[|5;6|]|]; 2|] : obj array

[<FSharkOutput>]
let outvalue = [|9;12|] : int array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
