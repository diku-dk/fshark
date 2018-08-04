module Zip4b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip4b (x : int array) (y : int array) : int array = 
    let x' = Zip4 x y x x
    let (a,b,c,d) = Unzip4 x'
    in b

[<FSharkInput>]
let value = [|
              [|1;2;3|] ;
              [|4;5;6|]
            |] : obj array

[<FSharkOutput>]
let outvalue = [|4;5;6|] : obj array

