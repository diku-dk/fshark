module Zip7a
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (x : int array) : int array =
    let x' = Zip7 x x x x x x x
    let (a,b,c,d,e,f,g) = Unzip7 x'
    in a

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3|] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
