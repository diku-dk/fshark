module Map1Single
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map1 (x : single array) : single array = Map ((+) 1.0f) x

[<FSharkInput>]
let value = [|[|1.5f; 2.5f; 3.5f|]|] : obj array

[<FSharkOutput>]
let outvalue = [|2.5f; 3.5f; 4.5f|] : single array

