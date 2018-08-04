namespace FShark.Compiler

module FSharkCompiler = 
    open System.IO
    open System
    open System.IO
    open FShark.IL.AST
    open FShark.IL.IL
    open FShark.Library.Utils
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    let rec GetPrimType (tp: FSharpType) : FSharkType =
        if tp.IsAbbreviation 
        then GetPrimType tp.AbbreviatedType
        else
            
        if tp.IsTupleType
        then let tps = List.map GetPrimType <| Seq.toList tp.GenericArguments
             match tps with
             | [FSharkArray _; FSharkArray (Prim (Int FInt64))] -> 
                failwith "The type ('a [] * int64 []) is reserved for FShark arrays"
             | _ -> FSharkTuple tps
             
        else if tp.HasTypeDefinition
        then let tp' = tp.TypeDefinition.DisplayName
             match tp' with
             | "Int" -> Prim <| Int FInt32
             | "Int8" -> Prim <| Int FInt8
             | "Byte" -> Prim <| UInt FUInt8
             | "SByte" -> Prim <| Int FInt8
             | "Int16" -> Prim <| Int FInt16
             | "Int32" -> Prim <| Int FInt32
             | "Int64" -> Prim <| Int FInt64
             | "UInt8" -> Prim <|  UInt FUInt8
             | "UInt16" -> Prim <| UInt FUInt16
             | "UInt32" -> Prim <| UInt FUInt32
             | "UInt64" -> Prim <| UInt FUInt64
             | "Single" -> Prim <| Float FSingle
             | "Double" -> Prim <| Float FDouble
             | "float" -> Prim <| Float FDouble
             | "float32" -> Prim <| Float FSingle
             | "Boolean" -> Prim FSharkPrim.Bool
             | "bool" -> Prim FSharkPrim.Bool
             | array when array = "array" || array = "[]" -> 
                let innerTp = tp.GenericArguments.[0]
                let innerTp' = GetPrimType innerTp
                in FSharkArray innerTp'
             | _ -> CustomType tp'
             
        else if tp.IsGenericParameter 
            then Polymorphic tp.GenericParameter.Name
            
        else failwithf "Type %s not supported" (tp.ToString())
    
    let rec CompileFSharkType (tp : FSharpType) : FSharkType =
        let rec UnravelFunctionType (tp : FSharpType) (tps : FSharkType list) : (FSharkType list * FSharkType) =
            if tp.IsFunctionType
            then let arg = CompileFSharkType tp.GenericArguments.[0]
                 let return_type = tp.GenericArguments.[1]
                 let tps' = tps @ [arg]
                 in UnravelFunctionType return_type tps'
            else let return_type = CompileFSharkType tp
                 in (tps, return_type)
                 
        match tp with
        | tp when tp.IsTupleType -> 
          let contents = Seq.map CompileFSharkType tp.GenericArguments
          FSharkTuple (Seq.toList contents)
          
        | tp when tp.IsFunctionType -> 
          let tp' = UnravelFunctionType tp []
          FSharkFunction tp'
          
        | tp ->
            GetPrimType tp
    
    let rec GetFSharkCode (e : FSharpExpr) : FSharkCode =
        match e with
        | BasicPatterns.Const(constValueObj, constType) -> 
            GetFSharkConst constValueObj constType
        | BasicPatterns.NewTuple(_, argExprs) -> 
            GetFSharkTuple argExprs
        | BasicPatterns.NewArray(arrayType, argExprs) -> 
            GetFSharkArray arrayType argExprs
        | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
            GetFSharkIf guardExpr thenExpr elseExpr
        | BasicPatterns.Call(_, memberOrFunc, _, typeArgs2, argExprs) -> 
            GetFSharkCall memberOrFunc typeArgs2 argExprs
        | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> 
            GetFSharkLambda lambdaVar bodyExpr
        | BasicPatterns.Value(valueToGet) -> 
            GetFSharkValue valueToGet
        | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> 
            GetFSharkLet bindingVar bindingExpr bodyExpr
        | BasicPatterns.TupleGet(_, tupleElemIndex, tupleExpr) -> 
            GetFSharkTupleGet tupleExpr tupleElemIndex
        | BasicPatterns.FSharpFieldGet(recordOpt, _, fieldInfo) -> 
            GetFSharkRecordGet recordOpt fieldInfo
        | BasicPatterns.NewRecord(recordType, argExprs) ->  
            GetFSharkRecord recordType argExprs
            
        | BasicPatterns.Application(funcExpr, _, argExprs) -> 
            GetFSharkApplication funcExpr argExprs
            
        | BasicPatterns.DecisionTree(_, _) -> 
            Pass
        | BasicPatterns.DecisionTreeSuccess (_, _) -> 
            Pass
            (*
        //| BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        //    visitObjArg f objExprOpt; visitExpr f argExpr
    
        //| GetFSharkRecordGet.WhileLoop(guardExpr, bodyExpr) -> 
            //GetFSharkWhile guardExpr bodyExpr
            *)
    
        | BasicPatterns.AddressOf(lvalueExpr) -> 
            GetFSharkCode lvalueExpr
        | BasicPatterns.TypeLambda(_, bodyExpr) -> 
            GetFSharkCode bodyExpr
            
        | _ -> failwith (sprintf "unrecognized %+A" e)
        (*
        | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
            visitExpr f lvalueExpr; visitExpr f rvalueExpr
        | BasicPatterns.Coerce(targetType, inpExpr) -> 
            visitExpr f inpExpr
        | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> 
            visitExpr f startExpr; visitExpr f limitExpr; visitExpr f consumeExpr
        | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
            visitExprs f argExprs
        | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
            visitObjArg f objExprOpt
        | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
            visitObjArg f objExprOpt
        | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
            List.iter (snd >> visitExpr f) recursiveBindings; visitExpr f bodyExpr
        | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
            visitExpr f delegateBodyExpr
        | BasicPatterns.NewObject(objType, typeArgs, argExprs) -> 
            visitExprs f argExprs
        | BasicPatterns.NewRecord(recordType, argExprs) ->  
            visitExprs f argExprs
        | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
            visitExprs f argExprs
        | BasicPatterns.Quote(quotedExpr) -> 
            visitExpr f quotedExpr
        | BasicPatterns.Sequential(firstExpr, secondExpr) -> 
            visitExpr f firstExpr; visitExpr f secondExpr
        | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> 
            visitExpr f bodyExpr; visitExpr f finalizeExpr
        | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> 
            visitExpr f bodyExpr; visitExpr f catchExpr
        | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
            visitExpr f tupleExpr
        | BasicPatterns.TypeTest(ty, inpExpr) -> 
            visitExpr f inpExpr
        | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->        visitExpr f unionExpr; visitExpr f valueExpr
        | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
            visitExpr f unionExpr
        | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
            visitExpr f unionExpr
        | BasicPatterns.UnionCaseTag(unionExpr, unionType) -> 
            visitExpr f unionExpr
        | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
            visitExpr f baseCallExpr
            List.iter (visitObjMember f) overrides
            List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
        | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) -> 
            visitExprs f argExprs
        | BasicPatterns.ValueSet(valToSet, valueExpr) -> 
            visitExpr f valueExpr
        | BasicPatterns.BaseValue baseType -> ()
        | BasicPatterns.DefaultValue defaultType -> ()
        | BasicPatterns.ThisValue thisType -> ()
        *)
    
    and GetFSharkVar (v : FSharpMemberOrFunctionOrValue) : FSharkVar = 
        let tp = CompileFSharkType v.FullType
        let varName = RemovePrefacingUnderscore v.CompiledName
        (varName, tp)
        
    and GetFSharkValue (v : FSharpMemberOrFunctionOrValue) : FSharkCode = 
        FSharkCode.Var <| RemovePrefacingUnderscore v.CompiledName
        
    and GetFSharkConst (v : obj) (tp : FSharpType) : FSharkCode =
        let tp' = GetPrimType tp
        FSharkCode.Const (v, tp')
        
    and GetFSharkTuple (exps : FSharpExpr list) : FSharkCode =
        let exps' = List.map GetFSharkCode exps
        FSharkCode.Tuple exps'
        
    and GetFSharkRecord (tp : FSharpType) (exps : FSharpExpr list) : FSharkCode =
        let recordFields = tp.TypeDefinition.FSharpFields
        let fieldNames = List.map (fst << GetRecordField) <| Seq.toList recordFields
        let exps' = List.map GetFSharkCode exps
        let fields = List.zip fieldNames exps'
        FSharkCode.Record fields
        
    and GetFSharkTupleGet tupleExpr (tupleElemIdx : int) : FSharkCode =
        let tupleExpr' = GetFSharkCode tupleExpr
        let tupleElemIdx' = tupleElemIdx+1 // F# zero indexes tuples, whilst Futhark one indexes
        FSharkCode.TupleGet (tupleExpr', tupleElemIdx')
            
    and GetFSharkArray tp exps : FSharkCode =
        let tp' = GetPrimType tp
        let exps' = List.map GetFSharkCode exps
        in FSharkCode.List (tp', exps')
        
    and GetFSharkIf guard then_branch else_branch : FSharkCode =
        let guard' = GetFSharkCode guard
        let then_branch' = GetFSharkCode then_branch
        let else_branch' = GetFSharkCode else_branch
        in FSharkCode.If(guard',then_branch',else_branch')
        
    and GetFSharkLet newVar newVal body : FSharkCode =
        let newVal' = GetFSharkCode newVal
        let body' = GetFSharkCode body
        let varName = RemovePrefacingUnderscore newVar.CompiledName
        FSharkCode.LetIn(Var varName, newVal', body')
        
    and GetFSharkCall (f : FSharpMemberOrFunctionOrValue) tps args : FSharkCode =
        let opOption = GetFSharkOp f tps args
        match opOption with
        | Some opCall -> opCall
        | _ ->
        
        let soacOption = GetFSharkSOAC f args
        match soacOption with
        | Some soacCall -> soacCall
        | _ ->
        
        let arrayGet = GetFSharkArrayIndexing f args
        match arrayGet with
        | Some arrCode -> arrCode
        | _ ->
        
        let fname = RemovePrefacingUnderscore f.CompiledName
        let args' = List.map GetFSharkCode args
        in FSharkCode.Call (fname, args')
            
    and GetArrayIndex (toCompile : FSharkCode) (args : FSharkCode list) : FSharkCode = 
        match toCompile with
        | FSharkCode.Const(_,_) -> FSharkCode.ArrayIndex(toCompile, args)
        | FSharkCode.Var _ -> FSharkCode.ArrayIndex(toCompile, args)
        | FSharkCode.ArrayIndex (arr, args') ->
            FSharkCode.ArrayIndex(arr, List.append args' args)
        | _ -> failwith "what"
    
    and GetFSharkArrayIndexing f args : FSharkCode option =
        let fname = RemovePrefacingUnderscore f.CompiledName
        match fname with
        | "GetArray" -> 
            let args' = List.map GetFSharkCode args
            in Some <| GetArrayIndex args'.[0] [args'.[1]]
        | _ -> None
            
    and GetFSharkApplication funcExpr argExprs : FSharkCode =
        let funcExpr' = GetFSharkCode funcExpr
        let argExprs' = List.map GetFSharkCode argExprs
        in FSharkCode.Application (funcExpr', argExprs')
        
    
    and GetFSharkOp (f : FSharpMemberOrFunctionOrValue) tps args : FSharkCode option =
        let fname = RemovePrefacingUnderscore f.CompiledName
        let getFtype = (Some << CompileFSharkType << List.head)
        let args' = List.map GetFSharkCode args
        match fname with
        | "op_Addition" -> Some <| FSharkCode.InfixOp (Add, getFtype tps, args'.[0], args'.[1])
        | "op_Subtraction" -> Some <| FSharkCode.InfixOp (Sub, getFtype tps, args'.[0], args'.[1])
        | "op_UnarySubtraction" -> Some <| FSharkCode.UnaryOp (UnarySub, getFtype tps, args'.[0])
        | "op_UnaryNegation" -> Some <| FSharkCode.UnaryOp (UnaryNeg, getFtype tps, args'.[0])
        | "op_Multiply" -> Some <| FSharkCode.InfixOp (Mul, getFtype tps, args'.[0], args'.[1])
        | "op_Division" -> Some <| FSharkCode.InfixOp (Div, getFtype tps, args'.[0], args'.[1])
        | "op_Modulus" -> Some <| FSharkCode.InfixOp (Mod, getFtype tps, args'.[0], args'.[1])
        | "op_Exponentiation" -> Some <| FSharkCode.InfixOp (Pow, getFtype tps, args'.[0], args'.[1])
        
        | "op_BooleanAnd" -> Some <| FSharkCode.InfixOp (LAnd, None, args'.[0], args'.[1])
        | "op_BooleanOr" -> Some <| FSharkCode.InfixOp (LOr, None, args'.[0], args'.[1])
        
        | "op_LessThan" -> Some <| FSharkCode.InfixOp (Less, getFtype tps, args'.[0], args'.[1])
        | "op_GreaterThan" -> Some <| FSharkCode.InfixOp (Greater, getFtype tps, args'.[0], args'.[1])
        | "op_LessThanOrEqual" -> Some <| FSharkCode.InfixOp (LEq, getFtype tps, args'.[0], args'.[1])
        | "op_GreaterThanOrEqual" -> Some <| FSharkCode.InfixOp (GEq, getFtype tps, args'.[0], args'.[1])
        | "op_Equality" -> Some <| FSharkCode.InfixOp (Eq, getFtype tps, args'.[0], args'.[1])
        | "op_Inequality" -> Some <| FSharkCode.InfixOp (NEq, getFtype tps, args'.[0], args'.[1])
        
        | "op_PipeLeft" -> Some <| FSharkCode.InfixOp (PipeL, None, args'.[0], args'.[1])
        | "op_PipeRight" -> Some <| FSharkCode.InfixOp (PipeR, None, args'.[0], args'.[1])
        | "op_Range" -> Some <| FSharkCode.Range(args'.[0], args'.[1])
        | _ -> GetFSharkOpFunction f tps args
        
    and getFtype = (CompileFSharkType << List.head)
    
    and GetCall f args = FSharkCode.Call(f, args)
    
    and GetTypedCall tps f args =
        let tp = getFtype tps
        in FSharkCode.TypedCall(tp, f, args)
    
    and GetFSharkOpFunction (f : FSharpMemberOrFunctionOrValue) tps args : FSharkCode option =
        if f.ApparentEnclosingEntity.FullName = "Microsoft.FSharp.Core.Operators" ||
           f.ApparentEnclosingEntity.FullName = "Microsoft.FSharp.Core.ExtraTopLevelOperators" ||
           f.ApparentEnclosingEntity.FullName = "Microsoft.FSharp.Collections.SeqModule"
        then 
            let fname = RemovePrefacingUnderscore f.CompiledName
            let args' = List.map GetFSharkCode args
            match fname with
            | "Identity" -> Some <| GetCall "id" args'
            | "Sqrt" -> Some <| GetTypedCall tps "sqrt" args'
            | "Abs" -> Some <| GetTypedCall tps "abs" args'
            | "Acos" -> Some <| GetTypedCall tps "acos" args'
            | "Asin" -> Some <| GetTypedCall tps "asin" args'
            | "Atan" -> Some <| GetTypedCall tps "atan" args'
            | "Atan2" -> Some <| GetTypedCall tps "atan2" args'
            | "Cos" -> Some <| GetTypedCall tps "cos" args'
            | "Sin" -> Some <| GetTypedCall tps "sin" args'
            | "Tan" -> Some <| GetTypedCall tps "tan" args'
            | "Compare" -> Some <| Compare args' tps
            | "Sinh" -> Some <| Sinh args' tps
            | "Cosh" -> Some <| Cosh args' tps
            | "Tanh" -> Some <| Tanh args' tps
            | "ToSingle" -> Some <| GetCall"f32.i64" [GetTypedCall tps "to_i64" args']
            | "ToDouble" -> Some <| GetCall"f64.i64" [GetTypedCall tps "to_i64" args']
            | "ToSByte"  -> Some <| GetCall"i8.i64"  [GetTypedCall tps "to_i64" args']
            | "ToByte"   -> Some <| GetCall"u8.i64"  [GetTypedCall tps "to_i64" args']
            | "ToInt"    -> Some <| GetCall"i32.i64" [GetTypedCall tps "to_i64" args']
            | "ToInt16"  -> Some <| GetCall"i16.i64" [GetTypedCall tps "to_i64" args']
            | "ToInt32"  -> Some <| GetCall"i32.i64" [GetTypedCall tps "to_i64" args']
            | "ToInt64"  -> Some <| GetCall"i64.i64" [GetTypedCall tps "to_i64" args']
            | "ToUInt16" -> Some <| GetCall"u16.i64" [GetTypedCall tps "to_i64" args']
            | "ToUInt32" -> Some <| GetCall"u32.i64" [GetTypedCall tps "to_i64" args']
            | "ToUInt64" -> Some <| GetCall"u64.i64" [GetTypedCall tps "to_i64" args']
            
            | "Exp" -> Some <| GetTypedCall tps "exp" args'
            | "Log" -> Some <| GetTypedCall tps "log" args'
            | "Log10" -> Some <| GetTypedCall tps "log10" args'
    
            | "Floor" -> Some <| GetTypedCall tps "floor" args'
            | "Ceiling" -> Some <| GetTypedCall tps "ceil" args'
            | "Round" -> Some <| GetTypedCall tps "round" args'
            | "Truncate" -> Some <| GetTypedCall tps "trunc" args'
            | "Fst" -> Some <| FSharkCode.TupleGet(List.head args', 1)
            | "Snd" -> Some <| FSharkCode.TupleGet(List.head args', 2)
            | Prefix "Infinity" _ -> Some <| GetTypedCall [f.FullType] "inf" []
            | Prefix "NaN" _ -> Some <| GetTypedCall [f.FullType] "nan" []
            
            | "Max" -> Some <| GetTypedCall tps "max" args'
            | "Min" -> Some <| GetTypedCall tps "min" args'
            | "Sign" -> Some <| GetCall "i32.i64" [GetTypedCall tps "to_i64" [GetTypedCall tps "sgn" args']]
            | "Not" -> Some <| GetCall "!" args'
            | "PowInteger" -> Some <| PowInteger args' tps
            | "Raise" -> Some <| GetCall "OOPS" []
            | "CreateSequence" -> Some <| GetFSharkSequence args'
            | "ToArray" -> Some <| FSharkCode.ToArray args'.[0]
            | _ -> failwithf "This operator ´%s´ is not (yet) supported." fname
            
        else None
    
    and GetFSharkSequence (range : FSharkCode list) : FSharkCode =
        match range with 
        | [Range(a, b)] -> Range(a,b)
        | _ -> failwith "eh"
        
    and Sinh args tps : FSharkCode =
        let ex = GetTypedCall tps "exp" [List.head args]
        let enegx = GetTypedCall tps "exp" [FSharkCode.UnaryOp(UnaryNeg, Some <| getFtype tps, List.head args)]
                                         
        let explusenegx = FSharkCode.InfixOp(Sub, Some <| getFtype tps, ex, enegx)
        in FSharkCode.InfixOp(Div, Some <| getFtype tps, explusenegx, FSharkCode.Const((upcast 2 : obj), getFtype tps))
        
    and Cosh args tps  : FSharkCode =
        let ex = GetTypedCall tps "exp" [List.head args]
        let enegx = 
            GetTypedCall tps "exp" [FSharkCode.UnaryOp(UnaryNeg, Some <| getFtype tps, List.head args)]
        
        let explusenegx = FSharkCode.InfixOp(Add, Some <| getFtype tps, ex, enegx)
        in FSharkCode.InfixOp(Div, Some <| getFtype tps, explusenegx, FSharkCode.Const((upcast 2 : obj), getFtype tps))
    
    and Tanh args tps : FSharkCode =
        let top = Sinh args tps 
        let bottom = Cosh args tps 
        in FSharkCode.InfixOp(Div, Some <| (CompileFSharkType << List.head) tps, top, bottom)
        
    and Tan args tps : FSharkCode =
        let top = GetTypedCall tps "sin" args
        let bottom = GetTypedCall tps "cos" args
        in FSharkCode.InfixOp(Div, Some <| (CompileFSharkType << List.head) tps, top, bottom)
        
    and Compare args tps : FSharkCode =
        let elseBranch = FSharkCode.If(FSharkCode.InfixOp(Less, Some <| getFtype tps, args.[0], args.[1]), 
                                           FSharkCode.Const((upcast -1 : obj), Prim <| FSharkPrim.Int FInt32),
                                           FSharkCode.Const((upcast 0 : obj), Prim <| FSharkPrim.Int FInt32))
        in FSharkCode.If(FSharkCode.InfixOp(Greater, Some <| getFtype tps, args.[0], args.[1]), 
                         FSharkCode.Const((upcast 1 : obj), Prim <| FSharkPrim.Int FInt32),
                         elseBranch)
                             
    and Log10 args tps : FSharkCode =
        let upper = GetTypedCall tps "log" args
        let lower = GetTypedCall tps "log" [FSharkCode.Const((upcast 10 : obj), getFtype tps)]
        in FSharkCode.InfixOp(Div, Some <| getFtype tps, upper, lower)
                             
    and PowInteger args tps : FSharkCode =
        let convertedExponent = GetTypedCall tps "i64" <| [GetTypedCall tps "to_i64" [args.[1]]]
        in FSharkCode.InfixOp(Pow, Some <| getFtype tps, args.[0], convertedExponent)
        
    and GetFSharkSOAC (f : FSharpMemberOrFunctionOrValue) args : FSharkCode option =
        if f.ApparentEnclosingEntity.DisplayName = "FSharkPrelude"
        then 
            let fname = RemovePrefacingUnderscore f.CompiledName
            let args' = List.map GetFSharkCode args
            let fname' = fname.ToLower()
            in Some <| FSharkCode.SOAC(fname', List.head args', List.tail args')
        else None
        // expand here with check on whether we are using a generic function with specific typed type params
        // i.e check if arg types are specific and function arg types are generic.
        // In that case, we will create an instantiated function call; i.e. f32.+ instead of just +
    
    and GetFSharkLambda (var : FSharpMemberOrFunctionOrValue) (bodyExpr : FSharpExpr) : FSharkCode =
        let var' = RemovePrefacingUnderscore var.CompiledName
        let tp = CompileFSharkType var.FullType
        let body' = GetFSharkCode bodyExpr
        in FSharkCode.Lambda (var', tp, body')
        
    and GetFSharkRecordGet (varOption : FSharpExpr option) field = 
        match varOption with
        | None -> failwith "what"
        | Some(var) ->
            let var' = GetFSharkCode var
            let (field', _) = GetRecordField field
            in FSharkCode.RecordGet(var', field')
        
    and CompileFSharkEntity (entity : FSharpEntity) subdecls : FSharkDecl =
        if entity.IsFSharpRecord
        then CompileFSharkRecord entity
        else 
        if entity.IsFSharpAbbreviation
        then CompileFSharkTypeAlias entity
        else 
        if entity.IsFSharpModule
        then 
            let name = RemovePrefacingUnderscore entity.CompiledName
            let subdecls' = List.map TraverseImplementationFile subdecls
            in FSharkDecl.FSharkModule (name, subdecls')
        else failwith "Not supported"
             
    and CompileFSharkRecord (entity: FSharpEntity) : FSharkDecl = 
        let name = RemovePrefacingUnderscore entity.CompiledName
        let fields = List.map GetRecordField <| Seq.toList entity.FSharpFields
        in FSharkRecord (name, fields)
        
    and CompileFSharkTypeAlias (entity : FSharpEntity) : FSharkDecl = 
        let name = RemovePrefacingUnderscore entity.CompiledName
        let tp = CompileFSharkType entity.AbbreviatedType
        in FSharkTypeAlias (name, tp)
        
    and GetRecordField (field : FSharpField) =
        let name = RemovePrefacingUnderscore field.Name
        let tp = CompileFSharkType field.FieldType
        in (name, tp)
        
    and TraverseImplementationFile (elm : FSharpImplementationFileDeclaration) : FSharkDecl =
            let isEntryAttribute (attribute : FSharpAttribute) : bool =
                attribute.AttributeType.CompiledName = "FSharkEntry"
                
            let isTestAttribute (attribute : FSharpAttribute) : bool =
                attribute.AttributeType.CompiledName = "FSharkInput" ||
                attribute.AttributeType.CompiledName = "FSharkOutput"
                
            match elm with
            | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
                CompileFSharkEntity entity subDecls
                   
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, exp) ->
                if v.IsMember then EmptyDecl else
                let attributes = Seq.toList v.Attributes
                if List.exists isTestAttribute attributes then EmptyDecl
                else 
                    let (name, tp) = GetFSharkVar v
                    let args' = List.map (List.map (fst << GetFSharkVar)) args
                    let exp' = GetFSharkCode exp
                    let isEntry = List.exists isEntryAttribute attributes
                    FSharkVal (isEntry, tp, name, args', exp')
            | FSharpImplementationFileDeclaration.InitAction(_) -> raise <| Exception ("InitActions are not supported in FShark.")
            | _ -> raise <| Exception ("This is not root.")



    let FSharkFromFSharpResults (input : FSharpCheckProjectResults) : FSharkDecl list =
        let rootEntity = input.AssemblyContents.ImplementationFiles.[0].Declarations.[0]
        match rootEntity with
        | FSharpImplementationFileDeclaration.Entity(_, containedDeclarations) ->
            let fsharkDecls = List.map TraverseImplementationFile containedDeclarations
            in fsharkDecls 
        | _ -> failwith "This shouldn't happen"

    let FSharkFromFSharpImplementationFileDeclaration (rootDecl : FSharpImplementationFileDeclaration) : FSharkDecl list =
        [TraverseImplementationFile rootDecl]
