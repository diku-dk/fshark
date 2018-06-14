module Map10
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map2 (x : int array array) : int array array = Map id x

[<FSharkInput>]
let value = [|[|
              [|1;2;3|] ;
              [|4;5;6|]
            |]|] : obj array

[<FSharkOutput>]
let outvalue = 
            [|
              [|1;2;3|] ;
              [|4;5;6|]
            |] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
