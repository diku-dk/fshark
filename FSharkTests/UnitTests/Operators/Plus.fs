module Plus
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let plus x y : int = x + y

[<FSharkInput>]
let value = [|4; 3|] : obj array

[<FSharkOutput>]
let sameValue = 7 : int

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
