module Reduce1
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (a : int array) : int = Reduce (*) 1 a

[<FSharkInput>]
let value = [|[|1;2;3;4;5;6;7;8;9|]|] : obj array

[<FSharkOutput>]
let outvalue = 362880 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
