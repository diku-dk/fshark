module Filter3
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

[<FSharkEntry>] // original test uses doubles but my CPU doesn't support f64s
let zip1a (xs1 : int array) (xs2 : bool array) : (bool array * int array) =
    let tmp = Filter (fun (x: (int * bool)) ->
                         let (i,b) = x in b
                      ) (Zip xs1 xs2) in
      Unzip(Map (fun (x: (int*bool))  ->
                  let (i,b) = x in (b,i)
               ) tmp)

[<FSharkInput>]
let value = [|[|0;1;-2;5;42|];[|false;true;true;false;true|]|] : obj array

[<FSharkOutput>]
let outvalue = ([|true;true;true|] , [|1;-2;42|]) : (bool [] * int [])
// fails until PrepareFSharkOutput works recursively on tuples

[<FSharkConvertion>]
let conv (x : obj) = downcast x : int
