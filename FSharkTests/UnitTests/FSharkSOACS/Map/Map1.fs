module Map1
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map1 (x : int array) : int array = Map ((+)1) x

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = [|2;3;4|] : int array

