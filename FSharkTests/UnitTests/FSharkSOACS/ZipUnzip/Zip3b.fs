module Zip3b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip3b (x : int array) (y : int array) : int array = 
    let x' = Zip3 x y x
    let (a,b,c) = Unzip3 x'
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
