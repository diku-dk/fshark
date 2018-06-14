namespace FShark.IL
module AST =
  type FSharkInt = FInt8
                 | FInt16
                 | FInt32
                 | FInt64
                 
  type FSharkUInt = FUInt8
                  | FUInt16
                  | FUInt32
                  | FUInt64
                  
  type FSharkFloat = FSingle
                   | FDouble
                   
  type FSharkPrim = Int of FSharkInt
                  | UInt of FSharkUInt
                  | Float of FSharkFloat
                  | Bool
                  
                  
  type FSharkExpr = Integer of int
                  | Decimal of float
                  | Bool of bool

  type FSharkType = Prim of FSharkPrim
                  | CustomType of string 
                  | Polymorphic of string
                  | FSharkTuple of FSharkType list
                  | FSharkArray of FSharkType
                  | FSharkFunction of (FSharkType list * FSharkType)
