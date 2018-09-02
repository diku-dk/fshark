module Matrix
open FSharkPrelude

[<FSharkEntry>]
let replicateRows n m = Replicate n [|1..m|]

[<FSharkEntry>]
let multiply A B =
  let BT = Transpose B
  let Sum = Foldr (+) 0
  in Map (fun row ->
       Map (fun col ->
         Sum <| Map2 (*) row col
       ) BT
     ) A