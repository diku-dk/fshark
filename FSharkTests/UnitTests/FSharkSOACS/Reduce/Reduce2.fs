module Reduce2
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let zip1a (a : int array) (b : int array): int = Reduce (+) 0 (Map2 (*) a b)

[<FSharkInput>]
let value = [|[|1;2;3;4;5;6;7;8;9|];
              [|1;2;3;4;5;6;7;8;9|]
            |] : obj array

[<FSharkOutput>]
let outvalue = 285 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
