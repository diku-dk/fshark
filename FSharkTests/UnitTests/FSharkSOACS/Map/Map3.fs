module Map13
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let map2 (x : int array array array) : int array array array = Map (Map (Map ((+)1))) x

[<FSharkInput>]
let value = [|
                [|
                    [|
                        [|1;2;3|] ;
                        [|4;5;6|]
                    |];
                    [|
                        [|7;8;9|] ;
                        [|10;11;12|]
                    |]
                |]
            |] : obj array

[<FSharkOutput>]
let outvalue =  [|
                    [|
                        [|2;3;4|] ;
                        [|5;6;7|]
                    |];
                    [|
                        [|8;9;10|] ;
                        [|11;12;13|]
                    |]
                |] : obj array

