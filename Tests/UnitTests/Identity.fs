namespace FShark.Tests
open FShark.Tests.TestBase
module Test =
    [<FSharkFun>]
    let identity (x : int) : int = x
    
    [<FSharkInput>]
    let value = 4 : int
    
    [<FSharkOutput>]
    let sameValue = 4 : int
    
    [<FSharkConvertion>]
    let conv (x : obj) = downcast x : int
 