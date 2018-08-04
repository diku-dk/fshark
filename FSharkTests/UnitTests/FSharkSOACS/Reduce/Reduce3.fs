module Reduce3
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (n : int) : (bool * int) = Reduce (fun (accx, accy) (x,y) -> (accx && x, y)) (false, 0) <| Zip (Replicate n true) (Replicate n 1)

[<FSharkInput>]
let value = [|0|] : obj array

[<FSharkOutput>]
let outvalue = (false, 0) : (bool * int)

