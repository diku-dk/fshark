module ReduceLambda
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (xs : int array) : int = Reduce (fun x y -> x * y) 1 xs

[<FSharkInput>]
let value = [|[|1;2;3|]|] : obj array

[<FSharkOutput>]
let outvalue = 6 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
