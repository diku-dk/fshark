namespace FShark.IL
module IL =
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FShark.IL.AST
    
    type Name = string
    type FSharkVar = (string * FSharkType)
    type FSharkUnaryOp = UnarySub
                       | UnaryNeg
    
    type FSharkBinOp = Add
                     | Sub
                     | Mul
                     | Div
                     | Mod
                     | Pow
                     
                     | Less
                     | LEq
                     | Greater
                     | GEq
                     | Eq
                     | NEq
                     
                     (*
                       bitwise things here? 
                     *)
                     
                     | LAnd
                     | LOr
                     | CompL
                     | CompR
                     | PipeL
                     | PipeR
                     
                     
    type FSharkBinBitOp = ShR
                        | ShL
                        | And
                        | Or
                        | XOr
                    
    type FSharkCode = Var of Name
                    | Const of (obj * FSharkType)
                    | Tuple of FSharkCode list
                    | Record of (Name * FSharkCode) list
                    | TupleGet of (FSharkCode * int)
                    | ArrayIndex of (FSharkCode * FSharkCode list)
                    | RecordGet of (FSharkCode * Name)
                    | List of (FSharkType * FSharkCode list)
                    | ToArray of FSharkCode
                    | If of (FSharkCode * FSharkCode * FSharkCode)
                    
                    | LetIn of (FSharkCode * FSharkCode * FSharkCode)
                    
                    | Call of (Name * FSharkCode list)
                    | TypedCall of (FSharkType * Name * FSharkCode list)
                    | Lambda of (Name * FSharkType * FSharkCode)
                    | Application of (FSharkCode * FSharkCode list)
                    
                    | UnaryOp of (FSharkUnaryOp * FSharkType option * FSharkCode)
                    | InfixOp of (FSharkBinOp * FSharkType option * FSharkCode * FSharkCode)
                    | InfixBitOp of (FSharkBinBitOp * FSharkType option * FSharkCode * FSharkCode)
                    | SOAC of (Name * FSharkCode * FSharkCode list)
                    | Range of (FSharkCode * FSharkCode)
                    | Sequence of (FSharkCode * FSharkCode * FSharkCode)
                    | Pass
                    //| Reduce of (FSharkCode * FSharkCode)
                    
                    //| While of (FSharkCode * FSharkCode)
                    //| For of (FSharkCode * FSharkCode)
                    // need to figure out something smart for 
                    // updating variable in Futhark context
    
    type FSharkDecl = FSharkVal of bool * FSharkType * Name * Name list list * FSharkCode
                    | FSharkTypeAlias of Name * FSharkType
                    | FSharkRecord of Name * (Name * FSharkType) list
                    | FSharkModule of Name * FSharkDecl list
                    | EmptyDecl
                    
    type FSharkProg = FSharkDecl list
    
    type FSharkTree = Entity of FSharpEntity * FSharkTree list
                    | Decl of FSharkDecl
