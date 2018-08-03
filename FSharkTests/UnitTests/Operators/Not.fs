module Not
// Not as in »this shirt is black not«
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes
open System

[<FSharkEntry>]
let nott (t : bool) (f : bool) : (bool * bool) =
    (not t , not f)
[<FSharkInput>]
let value = [|true; false|] : obj array

[<FSharkOutput>]
let sameValue = (false, true) : (bool * bool)


