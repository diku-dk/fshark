namespace FShark

module ArrayTests =
open FShark.Library.FSharkArrays

let (test_in : FSharkArray<int>) = 
    Dimension [
        Dimension [
            Bottom [|1;2;3;4;5|] ;
            Bottom [|6;7;8;9;10|]
        ];
        Dimension [
            Bottom [|11;12;13;14;15|];
            Bottom [|16;17;18;19;20|];
        ]
   ]
   
let (flattened, dims) = FSharkArrayToFlatArray<int> test_in
