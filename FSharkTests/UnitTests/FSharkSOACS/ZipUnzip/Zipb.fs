module Zip1b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1b (x : int array) (y : int array) : int array = 
    let x' = Zip x y
    let (a,b) = Unzip x'
    in b

[<FSharkInput>]
let value = [|
              [|1;2;3|] ;
              [|4;5;6|]
            |] : obj array

[<FSharkOutput>]
let outvalue = [|4;5;6|] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
