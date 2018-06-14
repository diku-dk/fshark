module Zip9b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip10b (x : int array) (y : int array) : int array =
    let x' = Zip10 x y x x x x x x x x
    let (a,b,c,d,e,f,g,h,i,j) = Unzip10 x'
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
