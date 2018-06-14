module Filter1
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (xs : int array) : int array = Filter ((<)0) xs

[<FSharkInput>]
let value = [|[|1;0;2;-5;3;-1|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1;2;3|] : int array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
