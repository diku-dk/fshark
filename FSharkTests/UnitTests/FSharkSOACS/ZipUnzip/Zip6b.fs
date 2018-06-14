module Zip6b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip6b (x : int array) (y : int array) : int array = 
    let x' = Zip6 x y x x x x
    let (a,b,c,d,e,f) = Unzip6 x'
    in b

[<FSharkInput>]
let value = [|
              [|1;2;3|] ;
              [|4;6;6|]
            |] : obj array

[<FSharkOutput>]
let outvalue = [|4;6;6|] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
