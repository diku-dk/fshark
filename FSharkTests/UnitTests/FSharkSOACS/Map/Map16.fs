module Map16
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let main (x: int)  : int array array = 
    Map (fun k -> if k = 0 then [|0|] else [|1|]) <| Iota x

[<FSharkInput>]
let value = [|2|] : obj array

[<FSharkOutput>]
let outvalue = [|[|0|];[|1|]|] : obj array

