module Zip3a
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (x : int array) : int array = 
    let x' = Zip3 x x x
    let (a,b,c) = Unzip3 x'
    in a

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3|] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
