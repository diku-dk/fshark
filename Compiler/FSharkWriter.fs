namespace FShark.Compiler

module FutharkWriter =
    open System
    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FShark.IL.AST
    open FShark.IL.IL
    open Internal.Utilities.StructuredFormat.TaggedTextOps
    open YC.PrettyPrinter
    open YC.PrettyPrinter.Doc
    open YC.PrettyPrinter.Pretty
    open YC.PrettyPrinter.StructuredFormat
    
    let FSharkPrimToString (p : FSharkPrim) : string =
        match p with
        | FSharkPrim.Int FInt8 -> "i8"
        | FSharkPrim.Int FInt16 -> "i16"
        | FSharkPrim.Int FInt32 -> "i32"
        | FSharkPrim.Int FInt64 -> "i64"
        | FSharkPrim.UInt FUInt8 -> "u8"
        | FSharkPrim.UInt FUInt16 -> "u16"
        | FSharkPrim.UInt FUInt32 -> "u32"
        | FSharkPrim.UInt FUInt64 -> "u64"
        | FSharkPrim.Float FSingle -> "f32"
        | FSharkPrim.Float FDouble -> "f64"
        | FSharkPrim.Bool -> "bool"
        
    let colonL = wordL ":"
    let betweenL e1 (sep : Doc) e2 = e1 >||< sep >||< e2
    let betweenL' e1 sep e2 = e1 >||< wordL sep >||< e2
        
    let rec PrettyPrintFSharkType (tp : FSharkType) : Doc =
        match tp with 
        | Prim p ->
            let p' = FSharkPrimToString p
            in wordL p'
        | Polymorphic str ->
            wordL <| "'"+str
        | FSharkTuple tps ->
            bracketL(commaListL <| List.map PrettyPrintFSharkType tps)
            
        | FSharkArray tp ->
            squareBracketL(emptyL) >|< PrettyPrintFSharkType tp
        
        | FSharkFunction(_, _) ->
            failwith "this shouldnt happen"
        | CustomType name -> wordL name
        
    let PrettyPrintFSharkArg (args, argtp) =
        let argtp' = PrettyPrintFSharkType argtp
        match args with 
        |[arg] -> 
           bracketL(wordL arg >||< colonL >||<  argtp')
        |args ->
            let args' = bracketL(commaListL <| List.map wordL args)
            bracketL(args' >||< colonL >||<  argtp')
        
    
    let rec PrettyPrintFSharkCode code =
        match code with
        | Var name ->
            wordL name
            
        | Const (obj, tp) ->
            match tp with
                 | Prim FSharkPrim.Bool -> wordL <| obj.ToString().ToLower()
                 | _ -> 
                    let tp' = PrettyPrintFSharkType tp
                    let obj' = obj.ToString()
                    wordL obj' >|< tp'
            
        | Tuple vals ->
            bracketL(commaListL <| List.map PrettyPrintFSharkCode vals)
            
        | Record vals ->
            let pprSetField(field,value) = wordL field >|< wordL "=" >|< PrettyPrintFSharkCode value
            in braceL(commaListL <| List.map pprSetField vals)
            
        | ToArray vals ->
            let vals' = PrettyPrintFSharkCode vals
            match vals with
            | Range _ -> vals'
            | _ -> squareBracketL vals'
            
        | TupleGet (tuple, idx) ->
            match tuple with
            |FSharkCode.Tuple _ -> failwith "FShark doesn't support getting values from tuple patterns. Assign tuple to variable first"
            | _ ->
            PrettyPrintFSharkCode tuple >|< wordL "." >|< (wordL <| idx.ToString())
            
        | RecordGet (record, field) ->
            PrettyPrintFSharkCode record >|< wordL "." >|< wordL field
            
        | List (_, vals) ->
            squareBracketL(commaListL <| List.map PrettyPrintFSharkCode vals)
            
        | Call (fname, []) ->
            wordL fname
            
        | Call (fname, vals) ->
            wordL fname >|< spaceListL(List.map (bracketL << PrettyPrintFSharkCode) vals)
            
        | TypedCall (tp, fname, vals) ->
            let call = PrettyPrintFSharkCode(Call(fname, vals))
            let tp' = PrettyPrintFSharkType tp
            in tp' >|< wordL "." >|< call
            
        | Lambda (name, tp, code) ->
            let tp' = PrettyPrintFSharkType tp
            in bracketL(wordL "\\" >|<
                bracketL(wordL name >||< colonL >||< tp') >||<
                wordL "->" >||< PrettyPrintFSharkCode code)
            
        | Application (code, args) ->
            let code' = PrettyPrintFSharkCode code
            let args' = List.map PrettyPrintFSharkCode args
            code' >|< bracketL(spaceListL args')
        
        | UnaryOp (op, tp, e1) ->
            bracketL(PrettyPrintFSharkUnaryOpExp op tp e1)
            
        | InfixOp (op, tp, e1, e2) ->
            bracketL(PrettyPrintFSharkBinOpExp op tp e1 e2)
            
        | SOAC (name, e, vals) ->
            let e' = PrettyPrintFSharkCode e
            let vals' = spaceListL <| List.map (bracketL << PrettyPrintFSharkCode) vals
            in wordL name >||< bracketL e' >||< vals'
            
        | LetIn (var, value, expr) ->
            let var' = PrettyPrintFSharkCode var
            let value' = PrettyPrintFSharkCode value
            let expr' = PrettyPrintFSharkCode expr
            in (wordL "let" >||< var' >||< wordL "=" >||< value' >||< wordL "in")
               @@ expr'
               
        | Range (a, b) ->
            let a' = PrettyPrintFSharkCode a
            let b' = PrettyPrintFSharkCode b
            in a' >|< wordL "..." >|< b'
            
        | Sequence (a, step, b) ->
            let a' = PrettyPrintFSharkCode a
            let b' = PrettyPrintFSharkCode b
            let step' = PrettyPrintFSharkCode step
            in a' >|< wordL ".." >|< step' >|< wordL "..." >|< b'
         
        | If (cond, thenBranch, elseBranch) ->
            let cond' = PrettyPrintFSharkCode cond
            let thenBranch' = PrettyPrintFSharkCode thenBranch
            let elseBranch' = PrettyPrintFSharkCode elseBranch
            
            in (wordL "if" >||< bracketL(cond')) @@--
                (wordL "then" @@-- bracketL(thenBranch')) @@
                (wordL "else" @@-- bracketL(elseBranch'))
                
        | Pass -> emptyL
                
        | ArrayIndex (arr, inds) ->
            let arr' = PrettyPrintFSharkCode arr
            let inds' = List.map PrettyPrintFSharkCode inds
            in arr' >|< squareBracketL(commaListL inds')
            
        | _ -> failwith "Aaaaaaaa"
        
    
    and PrettyPrintFSharkBinOpExp (p : FSharkBinOp) (tp : FSharkType option)(e1 : FSharkCode) (e2 : FSharkCode) : Doc =
        let e1' = PrettyPrintFSharkCode e1
        let e2' = PrettyPrintFSharkCode e2
    
        match p with
        | FSharkBinOp.PipeL -> e1' >||< bracketL(e2')
        | FSharkBinOp.PipeR -> e2' >||< bracketL(e1')
        | _ ->
            let p' = PrettyPrintFSharkInfixOp p
            in bracketL(e1' >||< p' >||< e2')
    
    and PrettyPrintFSharkInfixOp (p : FSharkBinOp) : Doc =
        let symbol = match p with
            | FSharkBinOp.Add -> "+"
            | FSharkBinOp.Sub -> "-"
            | FSharkBinOp.Mul -> "*"
            | FSharkBinOp.Div -> "/"
            | FSharkBinOp.Mod -> "%"
            | FSharkBinOp.Pow -> "**"
            | FSharkBinOp.LAnd-> "&&"
            | FSharkBinOp.LOr -> "||"
            | FSharkBinOp.Less -> "<"
            | FSharkBinOp.Greater -> ">"
            | FSharkBinOp.GEq -> ">="
            | FSharkBinOp.LEq -> "<="
            | FSharkBinOp.Eq -> "=="
            | FSharkBinOp.NEq -> "!="
            | _ -> failwith <| p.ToString()
        in wordL symbol
    
    
    and PrettyPrintFSharkUnaryOpExp (p : FSharkUnaryOp) (tp : FSharkType option) (e1 : FSharkCode) : Doc =
        let e1' = PrettyPrintFSharkCode e1
        let p' = PrettyPrintFSharkUnaryOp p
        let tp' = PrettyPrintFSharkTypeOption tp
        in tp' >|< p' >|< bracketL(e1')
    
    and PrettyPrintFSharkUnaryOp (p : FSharkUnaryOp) : Doc =
        match p with
        | FSharkUnaryOp.UnarySub -> wordL "-"
        | FSharkUnaryOp.UnaryNeg -> wordL "negate"
    
    and PrettyPrintFSharkTypeOption (opt : FSharkType option) : Doc =
        match opt with
        | None -> wordL ""
        | Some tp -> PrettyPrintFSharkType tp >|< wordL "."
    
    
    let StackDocs (docs : Doc list) : Doc = List.reduce (@@) docs
    
    let rec PrettyPrintFSharkDecl (unsafe : bool) (decl : FSharkDecl) : Doc =
        match decl with
        | FSharkVal (isEntry, FSharkFunction(argtypes, ret), name, args, body) ->
            let zippedArgs = List.zip args <| List.take (List.length args) argtypes
            let args' = spaceListL <| List.map PrettyPrintFSharkArg zippedArgs
            let retType = List.skip (List.length args) <| argtypes @ [ret]
            let ret' = sepListL (wordL "->") <| List.map PrettyPrintFSharkType retType
            let body' = let b = PrettyPrintFSharkCode body
                        in if unsafe && isEntry then wordL "unsafe" >||< b else b
            let funType = if isEntry then "entry" else "let"
            in (wordL funType >||< wordL name >||< args' >||< wordL ":" >||< ret' >||< wordL "=") @@-- body'
            
        | FSharkVal (_, tp, name, _, body) -> // shouldn't have args
            let tp' = PrettyPrintFSharkType tp
            let body' = PrettyPrintFSharkCode body
            in wordL "let" >||< wordL name >||< wordL ":" >||< tp' >||< wordL "=" >||< body'
    
        | FSharkTypeAlias (name, tp) ->
            let tp' = PrettyPrintFSharkType tp
            in wordL "type" >||< wordL name >||< wordL "=" >||< tp'
    
        | FSharkRecord(name, fields) ->
            let pprField(name, tp) =
                let tp' = PrettyPrintFSharkType tp
                wordL name >||< colonL >||< tp'
            let fields' = braceL(commaListL <| List.map pprField fields)
            in wordL "type" >||< wordL name >||< wordL "=" >||< fields'
            
        | FSharkModule(name, decls) ->
            let decls' = List.map (PrettyPrintFSharkDecl unsafe) decls
            //in wordL "module" >||< wordL name >||< wordL "=" @@-- (braceL <| StackDocs decls')
            in StackDocs decls'
            
        | EmptyDecl -> emptyL
            
        | _ -> failwith "unmatched FSharkDecl" 
    
    
    
    let FSharkDeclsToFuthark (decls : FSharkDecl list) (unsafe : bool) : string =
        let decls' = List.reduce (@@) <| List.map (PrettyPrintFSharkDecl unsafe) decls
        in print 80 decls'
