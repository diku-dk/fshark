namespace FSharkPreludeTests
module Tests =

open FSharkPrelude.FSharkPrelude
let RunTests =
    // test 1
    let MapTest =
        let arrs =  [|
                        [|1;2;3|]
                       ;[|4;5;6|]
                    |]
        let res = Map (fun arr -> Map ((+)1) arr) arrs
        let expected =  [|
                            [|2;3;4|]
                           ;[|5;6;7|]
                        |]
        in assert (res = expected); res = expected
    
    let Map2Test =
        let arrs =  [|
                        [|1;2;3|]
                       ;[|4;5;6|]
                    |]
        let res = Map2 (fun arr1 arr2 -> Map2 (fun x y -> x + y) arr1 arr2) arrs arrs
        let expected =  [|
                            [|2;4;6|]
                           ;[|8;10;12|]
                        |]
        in assert (res = expected); res = expected
    
    let ReduceTest =
        let res = Reduce (+) 0 [|1;2;3;4;5|]
        let expected = 15
        in assert (res = expected); res = expected
        
    let ReduceCommTest =
        let res = Reduce_Comm (+) 0 [|1;2;3;4;5|]
        let expected = 15
        in assert (res = expected); res = expected
        
        
    // test 5
    let ScanTest =
        let res = Scan (+) 0 [|1;2;3;4;5;6;7|]
        let expected = [|1;3;6;10;15;21;28|]
        in assert (res = expected); res = expected
        
    let FilterTest =
        let res = Filter (fun x -> x > 0) [|1;2;-3;4;0;6;7|]
        let expected = [|1;2;4;6;7|]
        in assert (res = expected); res = expected
    
    let PartitionTest =
        let res = Partition (fun x -> x > 0) [|1;2;-3;4;0;6;7|]
        let expected = ([|1;2;4;6;7|], [|-3;0|])
        in assert (res = expected); res = expected
    
    let Partition2Test =
        let res = Partition2 (fun x -> x < 0) ((=)0) [|1;2;-3;4;0;6;7|]
        let expected = ([|-3|], [|0|], [|1;2;4;6;7|])
        in assert (res = expected); res = expected
    
    let AllTest = 
        let res = All ((=) 0) [|1;2;-3;4;0;6;7|]
        let expected = false
        in assert (res = expected); res = expected
    
    // test 10
    let AllTest2 = 
        let res = All ((<>) 0) [|-10000;1;2;-3;4;6;7|]
        let expected = true
        in assert (res = expected); res = expected
    
    let AllTest3 = 
        let res = All ((<>) 'a') [|'f';'u';'n';'k';'y'|]
        let expected = true
        in assert (res = expected); res = expected
    
    let AnyTest = 
        let res = Any ((=) 0) [|1;2;-3;4;0;6;7|]
        let expected = true
        in assert (res = expected); res = expected
    
    let AnyTest2 = 
        let res = Any ((=) 0) [|-10000;1;2;-3;4;6;7|]
        let expected = false
        in assert (res = expected); res = expected
    
    let AnyTest3 = 
        let res = Any ((<>) 'a') [|'f';'u';'n';'k';'y'|]
        let expected = true
        in assert (res = expected); res = expected
    
    let AnyTest4 = 
        let res = Any ((=) 'a') [|'f';'u';'n';'k';'a';'y'|]
        let expected = true
        in assert (res = expected); res = expected
    
    let ScatterTest =
        let res = Scatter [|1;2;0;4;5;0;0;8;9|] [|2;5;6|] [|3;6;7|]
        let expected = [|1;2;3;4;5;6;7;8;9|]
        in assert (res = expected); res = expected
    
    let FoldlTest1 =
        let res = Foldl (fun acc value -> acc - value) 0 [|1;2;3;4;5;6;7;8;9|]
        let expected = -45
        in assert (res = expected); res = expected
    
    let FoldlTest2 =
        let res = Foldl (fun acc value -> acc - value) 9 [||]
        let expected = 9
        in assert (res = expected); res = expected
    
    let FoldrTest1 =
        let res = Foldr (fun value acc -> acc - value) 0 [|1;2;3;4;5;6;7;8;9|]
        let expected = -45
        in assert (res = expected); res = expected
    
    let FoldrTest2 =
        let res = Foldr (fun value acc -> value - acc) 0 [|1;2;3;4;5;6;7;8;9|]
        let expected = 5
        in assert (res = expected); res = expected
    
    let LengthTest1 =
        let res = Length [|1;2;3;4;5;6;7;8;9|]
        let expected = 9
        in assert (res = expected); res = expected
    
    let LengthTest2 =
        let res = Length [||]
        let expected = 0
        in assert (res = expected); res = expected
    
    let NullTest1 =
        let res = Null [|1|]
        let expected = false
        in assert (res = expected); res = expected
    
    let NullTest2 =
        let res = Null [||]
        let expected = true
        in assert (res = expected); res = expected
    
    let HeadTest1 =
        let res = Head [|1;2;3;4;5|]
        let expected = 1
        in assert (res = expected); res = expected
    
    let HeadTest2 =
        let arrs =  [|
                        [|1;2;3|]
                       ;[|4;5;6|]
                    |]
        let res = Head arrs
        let expected = [|1;2;3|]
        in assert (res = expected); res = expected
    
    let LastTest1 =
        let res = Last [|1;2;3;4;5|]
        let expected = 5
        in assert (res = expected); res = expected
    
    let LastTest2 =
        let arrs =  [|
                        [|1;2;3|]
                       ;[|4;5;6|]
                    |]
        let res = Last arrs
        let expected = [|4;5;6|]
        in assert (res = expected); res = expected
    
    let TailTest1 =
        let res = Tail [|1;2;3;4;5|]
        let expected = [|2;3;4;5|]
        in assert (res = expected); res = expected
    
    let TailTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|]
                    |]
        let res = Tail arrs
        let expected =  [|
                            [|4;5;6|];
                            [|7;8;9|]
                        |]
        in assert (res = expected); res = expected
    
    let InitTest1 =
        let res = Init [|1;2;3;4;5|]
        let expected = [|1;2;3;4|]
        in assert (res = expected); res = expected
    
    let InitTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|]
                    |]
        let res = Init arrs
        let expected =  [|
                            [|1;2;3|];
                            [|4;5;6|]
                        |]
        in assert (res = expected); res = expected
    
    let TakeTest1 =
        let res = Take 3 [|1;2;3;4;5|]
        let expected = [|1;2;3|]
        in assert (res = expected); res = expected
    
    let TakeTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|]
                    |]
        let res = Take 1 arrs
        let expected =  [|
                            [|1;2;3|]
                        |]
        in assert (res = expected); res = expected
    
    let DropTest1 =
        let res = Drop 3 [|1;2;3;4;5|]
        let expected = [|4;5|]
        in assert (res = expected); res = expected
    
    let DropTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|]
                    |]
        let res = Drop 2 arrs
        let expected =  [|
                            [|7;8;9|]
                        |]
        in assert (res = expected); res = expected
    
    let SplitTest1 =
        let res = Split 3 [|1;2;3;4;5|]
        let expected = ([|1;2;3|],[|4;5|])
        in assert (res = expected); res = expected
    
    let SplitTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|]
                    |]
        let res = Split 1 arrs
        let expected =  
                        ([|
                            [|1;2;3|]
                         |],
                        [|
                            [|4;5;6|];
                            [|7;8;9|]
                        |])
        in assert (res = expected); res = expected
    
    let Split2Test1 =
        let res = Split2 3 6 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = ([|1;2;3|],[|4;5;6|], [|7;8;9;10|])
        in assert (res = expected); res = expected
    
    let Split2Test2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|];
                        [|7;8;9|]
                    |]
        let res = Split2 1 2 arrs
        let expected =  
                        ([|
                            [|1;2;3|]
                         |],
                        [|
                            [|4;5;6|]
                        |],
                        [|
                            [|7;8;9|];
                            [|7;8;9|]
                        |])
        in assert (res = expected); res = expected
    
    let ReverseTest1 =
        let res = Reverse [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|10;9;8;7;6;5;4;3;2;1|]
        in assert (res = expected); res = expected
    
    let ReverseTest2 =
        let arrs =  [|
                        [|1;2;3|];
                        [|4;5;6|];
                        [|7;8;9|];
                        [|7;8;9|]
                    |]
        let res = Reverse arrs
        let expected =  [|
                            [|7;8;9|];
                            [|7;8;9|];
                            [|4;5;6|];
                            [|1;2;3|]
                        |]
        in assert (res = expected); res = expected
    
    let RotateTest1 =
        let res = Rotate 2 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|3;4;5;6;7;8;9;10;1;2|]
        in assert (res = expected); res = expected
    
    let RotateTest2 =
        let res = Rotate 9 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|10;1;2;3;4;5;6;7;8;9|]
        in assert (res = expected); res = expected; res = expected 
    
    let RotateTest3 =
        let res = Rotate 100 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|1;2;3;4;5;6;7;8;9;10|]
        in assert (res = expected); res = expected
    
    let RotateTest4 =
        let res = Rotate (-100) [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|1;2;3;4;5;6;7;8;9;10|]
        in assert (res = expected); res = expected
    
    let RotateTest5 =
        let res = Rotate -2 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|9;10;1;2;3;4;5;6;7;8|]
        in assert (res = expected); res = expected
    
    let RotateTest6 =
        let res = Rotate -9 [|1;2;3;4;5;6;7;8;9;10|]
        let expected = [|2;3;4;5;6;7;8;9;10;1|]
        in assert (res = expected); res = expected
    
    let UpdateTest =
        let res = Update [|1;2;3;4;5;6;7;8;9|] 4 999 
        let expected = [|1;2;3;4;999;6;7;8;9|]
        in assert (res = expected); res = expected
    
    let IotaTest =
        let res = Iota 10
        let expected = [|0;1;2;3;4;5;6;7;8;9|]
        in assert (res = expected); res = expected
    
    let ReplicateTest =
        let res = Replicate 10 'a'
        let expected = [|'a';'a';'a';'a';'a';'a';'a';'a';'a';'a'|]
        in assert (res = expected); res = expected
    
    let FlattenTest1 =
        let res = Flatten [|[|1;2;3|];[|4;5|];[|6;7;8;9|];[|10|]|]
        let expected = [|1;2;3;4;5;6;7;8;9;10|]
        in assert (res = expected); res = expected
    
    let FlattenTest2 =
        let arrs =  [|
                    [|
                        [|1;2;3|];
                        [|4;5;6|]
                    |];
                    [|
                        [|7;8;9|];
                        [|7;8;9|]
                    |]
                    |]
        let res = Flatten arrs
        let expected = [|[|1;2;3|];[|4;5;6|];[|7;8;9|];[|7;8;9|]|]
        in assert (res = expected); res = expected
    
    let Flatten_3DTest1 =
        let arrs =  [|
                    [|
                        [|1;2;3|];
                        [|4;5;6|]
                    |];
                    [|
                        [|7;8;9|];
                        [|7;8;9|]
                    |]
                    |]
        let res = Flatten_3D arrs
        let expected = [|1;2;3;4;5;6;7;8;9;7;8;9|]
        in assert (res = expected); res = expected
    
    let TransposeTest1 =
        let matrix = 
                    [|
                        [|
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |]
                        |];
                        [|
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |]
                        |];
                    |]
        let res = Transpose matrix
        let expected =
                    [|
                        [|
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |]
                        |];
                        [|
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |]
                        |];
                        [|
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |];
                            [|
                                [|1;2;3|];
                                [|4;5;6|];
                                [|7;8;9|]
                            |]
                        |]
                    |]
        in assert (res = expected); res = expected 
    printfn "%A" [MapTest;
        Map2Test;
        ReduceTest;
        ReduceCommTest;
        ScanTest;
        FilterTest;
        PartitionTest;
        Partition2Test;
        AllTest; 
        AllTest2; 
        AllTest3; 
        AnyTest; 
        AnyTest2; 
        AnyTest3; 
        AnyTest4; 
        ScatterTest;
        FoldlTest1;
        FoldlTest2;
        FoldrTest1;
        FoldrTest2;
        LengthTest1;
        LengthTest2;
        NullTest1;
        NullTest2;
        HeadTest1;
        HeadTest2;
        LastTest1;
        LastTest2;
        TailTest1;
        TailTest2;
        InitTest1;
        InitTest2;
        TakeTest1;
        TakeTest2;
        DropTest1;
        DropTest2;
        SplitTest1;
        SplitTest2;
        Split2Test1;
        Split2Test2;
        ReverseTest1;
        ReverseTest2;
        RotateTest1;
        RotateTest2;
        RotateTest3;
        RotateTest4;
        RotateTest5;
        RotateTest6;
        UpdateTest;
        IotaTest;
        ReplicateTest;
        FlattenTest1;
        FlattenTest2;
        Flatten_3DTest1;
        TransposeTest1]
        
