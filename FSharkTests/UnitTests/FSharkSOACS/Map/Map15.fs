module Map15
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let main (x: int) (a: int array) : int array = Map ((+)x) a

[<FSharkInput>]
let value = [|5;[|1;2;3;4|]|] : obj array

[<FSharkOutput>]
let outvalue = [|6;7;8;9|] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
