module ExampleModule
open FSharkPrelude

module SomeValues =
  let Four : int = 4
  let SomePlus (x : int) (y : int) : int = x + y
  
  module InnerModule =
    let Four2 : int = 4
    let SomePlus2 (x : int) (y : int) : int = x + y
  
[<FSharkEntry>]
let Doot (x : int) : int = x + x
  
let TimesTwo (x : int) : int =
  Doot x

[<FSharkEntry>]
let PlusSeven (x : int) : int =
  SomeValues.InnerModule.SomePlus2 x 7
  
[<FSharkEntry>]
let PlusTwo (x : int) : int =
  SomeValues.SomePlus x 2
  
  
