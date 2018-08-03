module Range
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let range : (int8 array * int16 array * int array * int64 array * single array * double array ) = 
  ( 
    [|1y..10y|]
  , [|1s..10s|]
  , [|1..10|]
  , [|1L..10L|]
  , [|1.0f..10.0f|]
  , [|1.0..10.0|])

[<FSharkInput>]
let value = [||] : obj array

[<FSharkOutput>]
let sameValue = (
[|1y; 2y; 3y; 4y; 5y; 6y; 7y; 8y; 9y; 10y|] ,
[|1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s; 9s; 10s|] ,
[|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|] ,
[|1L; 2L; 3L; 4L; 5L; 6L; 7L; 8L; 9L; 10L|] ,
[|1.0f; 2.0f; 3.0f; 4.0f; 5.0f; 6.0f; 7.0f; 8.0f; 9.0f; 10.0f|],
[|1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|] ) : (int8 array * int16 array * int array * int64 array * single array * double array)

