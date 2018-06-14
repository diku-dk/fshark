module Map11
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map2 (xs : int array) : int array array = Map (fun n -> Iota n) xs

[<FSharkInput>]
let value = [|[|8;8;8;8|]|] : obj array

[<FSharkOutput>]
let outvalue = 
            [|
              [|0;1;2;3;4;5;6;7|] ;
              [|0;1;2;3;4;5;6;7|] ;
              [|0;1;2;3;4;5;6;7|] ;
              [|0;1;2;3;4;5;6;7|]
            |] : obj array

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
