module Zip1a
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (x : int array) : int array = 
    let x' = Zip x x
    let (a,b) = Unzip x'
    in a

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3|] : obj array

