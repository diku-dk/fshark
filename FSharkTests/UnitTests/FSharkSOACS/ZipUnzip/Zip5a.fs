module Zip5a
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (x : int array) : int array =
    let x' = Zip5 x x x x x
    let (a,b,c,d,e) = Unzip5 x'
    in a

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3|] : obj array

