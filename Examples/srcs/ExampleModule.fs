module ExampleModule
open FSharkPrelude

module SomeValues =
  let Four : int = 4

  let SomePlus (x : int) (y : int) : int = x + y

[<FSharkEntry>]
let TimesTwo (x : int) : int =
  SomeValues.SomePlus x x
  
[<FSharkEntry>]
let MapPlusTwo (xs : int array) : int array =
  Map ((+)2) xs

let PlusSeven (x : int) : int =
  SomeValues.SomePlus x 7
