namespace FShark.TestTypes

[<AutoOpen>]
module TestTypes =
    (* Types we need for parsing the unit tests *)
    type FSharkFun() = inherit System.Attribute()
    type FSharkInput() = inherit System.Attribute()
    type FSharkOutput() = inherit System.Attribute()
    type FSharkCases() = inherit System.Attribute()
    type FSharkConvertion() = inherit System.Attribute()
    
