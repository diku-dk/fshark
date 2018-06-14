module Reduce0
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (n : int) : int = Reduce (+) 0 (Iota n)

[<FSharkInput>]
let value = [|100|] : obj array

[<FSharkOutput>]
let outvalue = 4950 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
