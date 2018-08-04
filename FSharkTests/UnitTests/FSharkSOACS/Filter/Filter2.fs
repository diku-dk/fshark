module Filter2
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>] // original test uses doubles but my CPU doesn't support f64s
let zip1a (a0 : single array) (a1 : int array) (oks : bool array) : single array =
    let (b, _) = Unzip <| Filter (fun (x : (single * int)) ->
                                    let (_, i) = x
                                    in oks.[i]) (Zip a0 a1)
    in b

[<FSharkInput>]
let value = [|[|1.0f;2.4f;5.3f|];[|0;1;2|];[|true;false;true|]|] : obj array

[<FSharkOutput>]
let outvalue = [|1.0f;5.3f|] : single array

