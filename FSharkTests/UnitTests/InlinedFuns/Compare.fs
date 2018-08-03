module Compare
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let comppare (x : int) (y : int) : (int * int * int) =
    (compare x y,
     compare x x,
     compare y x)

[<FSharkInput>]
let value = [|1;2|] : obj array

[<FSharkOutput>]
let sameValue = (-1, 0, 1) : (int * int * int)
