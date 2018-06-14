module Map12
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map2 (x : int array array) : int array array = Map (Map ((+)1)) x

[<FSharkInput>]
let value = [|[|
              [|1;2;3|] ;
              [|4;5;6|]
            |]|] : obj array

[<FSharkOutput>]
let outvalue = 
            [|
              [|2;3;4|] ;
              [|5;6;7|]
            |] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
