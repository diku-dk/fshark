module Id
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let id (v : int) : int = id v
 
[<FSharkInput>]
let value = [|5|] : obj array

[<FSharkOutput>]
let sameValue = 5 : int

