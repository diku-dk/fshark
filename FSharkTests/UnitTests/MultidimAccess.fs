module MultidimAccess
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (n : int) : int = 
    let arr = [|[|1;2;3|];[|4;5;6|]|]
    in arr.[0].[1]

[<FSharkInput>]
let value = [|0|] : obj array

[<FSharkOutput>]
let outvalue = 2 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
