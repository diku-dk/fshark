module Map14
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>]
let main (x: int) (y: int) (a: int array) (b: int array): int array =
  let c = Map (fun (av: int)  ->
                let v = x + y in
                (v, 2*av)) a in
  Map (fun (x,y) -> x+y) c

[<FSharkInput>]
let value = [|5; 5; [|1;2;3;4|]; [|1;2|]|] : obj array

[<FSharkOutput>]
let outvalue = [|12;14;16;18|] : obj array

