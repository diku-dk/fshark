module Zip8b
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip8b (x : int array) (y : int array) : int array =
    let x' = Zip8 x y x x x x x x
    let (a,b,c,d,e,f,g,h) = Unzip8 x'
    in b

[<FSharkInput>]
let value = [|
              [|1;2;3|] ;
              [|4;5;6|]
            |] : obj array

[<FSharkOutput>]
let outvalue = [|4;5;6|] : obj array

