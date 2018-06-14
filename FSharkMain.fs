namespace FShark.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Reflection

open FShark.IL
open FShark.Compiler
open FShark.Library.Utils
open FShark.Library.FSharkArrays

module FSharkMain =
    type FSharkMain = class
        val mutable InputFunctions : Map<string,string list>
        val mutable CompiledFunctions : Map<string,MethodInfo>
        val mutable IsCompiled : bool
        val mutable OpenCL : bool
        val mutable LibraryName : string
        val mutable LibraryRoot : string
        val mutable LibraryPath : string
        val mutable PreludePath : string
        val mutable MonoOptionsPath : string
        val mutable ClooPath : string
        val mutable LibraryInstance : obj
        val mutable LibraryArgs : string array
        val mutable ImportFiles : string list
        val mutable Unsafe : bool
        abstract member AddSourceFile : string -> unit
        abstract member AddImportFile : string -> unit
        
        abstract member CompileAndLoad : unit
        abstract member CompileFunctions : unit
        
        abstract member GetCompiledFunction  : string -> MethodInfo
        abstract member InvokeFunction<'T> : string -> obj[] -> obj
        
        new ((libName:string),(libRoot : string),(clooPath : string),
            (monoPath : string),(preludePath : string),(openCL : bool), 
            (unsafe : bool)) =
                { InputFunctions = Map.empty
                ; CompiledFunctions = Map.empty
                ; IsCompiled = false
                ; LibraryName = libName
                ; LibraryRoot = libRoot
                ; LibraryPath = CreateTempLibDir libRoot
                ; PreludePath = preludePath
                ; ClooPath = clooPath
                ; MonoOptionsPath = monoPath
                ; LibraryInstance = null
                ; LibraryArgs = Array.empty
                ; ImportFiles = []
                ; OpenCL = openCL
                ; Unsafe = unsafe
                }
            
            
        default this.AddSourceFile filepath : unit = do
            let file = Seq.toList <| System.IO.File.ReadLines(filepath)
            let file' = 
                [String.Format("(* Start of SourceFile {0} *)", filepath)] @
                file @
                [String.Format("(* End of SourceFile {0} *)", filepath)]
                
            in this.InputFunctions <- Map.add filepath file' this.InputFunctions
            
        default this.AddImportFile filepath = do
            let file = Seq.toList <| System.IO.File.ReadLines(filepath)
            let file' = 
                [String.Format("(* Start of SourceFile {0} *)", filepath)] @
                file @
                [String.Format("(* End of SourceFile {0} *)", filepath)]
                
            in this.ImportFiles <- List.append this.ImportFiles file'
            
        default this.CompileAndLoad = 
            this.LibraryPath <- CreateTempLibDir this.LibraryRoot
            let srcs = this.ConcatenateSources
            
            // write compiled source to some designation
            
            // attempt compilation of compiled source, write compilation errors to stdout if mistake and quit.
            
            let parsedFile = FSharkParser.ParseAndCheckSingleFile(srcs, this.PreludePath)
            if parsedFile.HasCriticalErrors 
            then CompilePanic parsedFile.Errors
            else
            let decls = FSharkCompiler.FSharkFromFSharpResults(parsedFile)
            let futharkSrc = FutharkWriter.FSharkDeclsToFuthark decls this.Unsafe
            let futharkPath = this.GetPathWithSuffix this.LibraryPath this.LibraryName "fut"
            let futharkOutPath = this.GetPathWithoutSuffix
            let futharkCSPath = System.IO.Path.ChangeExtension(futharkOutPath, "cs")
            let futharkDLLPath = System.IO.Path.ChangeExtension(futharkOutPath, "dll")
            this.WriteSourceToPath futharkPath futharkSrc
            COMPILE_SUCCESS(this.CompileFutharkModule futharkPath this.OpenCL)
            
            this.CompileAndLoadCSModule futharkCSPath futharkDLLPath
        
        member this.WriteSourceToPath (source:string) (path:string) =
            System.IO.File.WriteAllText(path, source)
            
        default this.CompileFunctions =
            let path = System.IO.Path.GetTempFileName()
            let outpath = path + ".dll"
            let function_sources = this.ConcatenateSources
            let function_class_src = this.WrapInClass function_sources
            ignore <| this.WriteSourceToPath function_class_src path
            
            this.CompileAndLoadCSModule path outpath
                
        default this.GetCompiledFunction (fname : string) =
            Map.find fname this.CompiledFunctions
            
        member private this.CreateInt8Array (data : 'b []) (lens : int64 []) : obj =
                    let data8 = Array.map unbox<int8> data
                    in (data8, lens) :> obj
        
        member private this.CreateInt16Array (data : 'b []) (lens : int64 []) : obj =
                    let data16 = Array.map unbox<int16> data
                    in (data16, lens) :> obj
                    
        member private this.CreateInt32Array (data : 'b []) (lens : int64 []) : obj =
                    let data32 = Array.map unbox<int32> data
                    in (data32, lens) :> obj
        
        member private this.CreateInt64Array (data : 'b []) (lens : int64 []) : obj =
                    let data64 = Array.map unbox<int64> data
                    in (data64, lens) :> obj
                    
        member private this.CreateUInt8Array (data : 'b []) (lens : int64 []) : obj =
                    let data8 = Array.map unbox<uint8> data
                    in (data8, lens) :> obj
        
        member private this.CreateUInt16Array (data : 'b []) (lens : int64 []) : obj =
                    let data16 = Array.map unbox<uint16> data
                    in (data16, lens) :> obj
                    
        member private this.CreateUInt32Array (data : 'b []) (lens : int64 []) : obj =
                    let data32 = Array.map unbox<uint32> data
                    in (data32, lens) :> obj
        
        member private this.CreateUInt64Array (data : 'b []) (lens : int64 []) : obj =
                    let data64 = Array.map unbox<uint64> data
                    in (data64, lens) :> obj
        member private this.CreateF32Array (data : 'b []) (lens : int64 []) : obj =
                    let data64 = Array.map unbox<single> data
                    in (data64, lens) :> obj
                    
        member private this.CreateF64Array (data : 'b []) (lens : int64 []) : obj =
                    let data64 = Array.map unbox<double> data
                    in (data64, lens) :> obj
                    
        member private this.CreateBoolArray (data : 'b []) (lens : int64 []) : obj =
                    let databool = Array.map unbox<bool> data
                    in (databool, lens) :> obj
            
        member this.PrepareFSharkInput (variable : obj) : obj =
            let tp = variable.GetType()
            if tp.IsArray then 
                let (data, lens) = ArrayToFlatArray (variable :?> System.Array)
                match GetBottomType (data.[0]) with
                | "System.Int8" -> 
                    this.CreateInt8Array data lens
                | "System.Int16" -> 
                    this.CreateInt16Array data lens
                | "System.Int32" -> 
                    this.CreateInt32Array data lens
                | "System.Int64" -> 
                    this.CreateInt64Array data lens
                | "System.UInt8" -> 
                    this.CreateUInt8Array data lens
                | "System.UInt16" -> 
                    this.CreateUInt16Array data lens
                | "System.UInt32" -> 
                    this.CreateUInt32Array data lens
                | "System.UInt64" -> 
                    this.CreateUInt64Array data lens
                | "System.Single" -> 
                    this.CreateF32Array data lens
                | "System.Double" -> 
                    this.CreateF64Array data lens
                | "System.Boolean" -> 
                    this.CreateBoolArray data lens
                | what -> failwithf "%s" what
            else    
                variable
            
        member private this.RestoreFlatArray (variable : ('a [] * int64 [])) : obj =
            let (data, dims) = variable
            in FlatArrayToFSharkArray data dims
            
        member this.PrepareFSharkOutput (variable : obj) : obj =
            if FSharpType.IsTuple <| variable.GetType()
            then
                match variable with 
                | :? (int8 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (int16 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (int [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (int64 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (uint8 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (uint16 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (uint32 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (uint64 [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (single [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (double [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | :? (bool [] * int64[]) as someTuple ->
                    this.RestoreFlatArray someTuple
                | _ -> this.HandleTupleOutput variable
            else    
                variable
            
        member private this.HandleTupleOutput tuple : obj = 
            let tupleFields = FSharpValue.GetTupleFields tuple
            let tupleFields' = Array.map this.PrepareFSharkOutput tupleFields
            let tupleTypes = Array.map (fun x -> x.GetType()) tupleFields'
            let tupleType = FSharpType.MakeTupleType tupleTypes
            let tupleFields' = Array.map this.PrepareFSharkOutput tupleFields
            in FSharpValue.MakeTuple(tupleFields', tupleType)
            
        default this.InvokeFunction(str : string) (parameters : obj array) =
            let parameters' = (Array.map this.PrepareFSharkInput) parameters
            let (method : MethodInfo) = this.GetCompiledFunction str
            let result = method.Invoke(this.LibraryInstance, parameters')
            let result' = this.PrepareFSharkOutput result
            in result'
            
        member private this.CompileCSModule (sourcePath : string) (targetPath : string) : unit =
            COMPILE_SUCCESS(this.CompileCSharpModule sourcePath targetPath)
        
        member private this.CompileAndLoadCSModule (sourcePath : string) (targetPath : string) : unit =
            this.CompileCSModule sourcePath targetPath
            this.CopyExternalLibsToDir
            this.IsCompiled <- true
            this.LoadCompiledModule(targetPath) 
             
        member private this.CopyExternalLibsToDir : unit =
            let copyFile file targetDir =
                let targetFile = String.Format("{0}/{1}", targetDir, System.IO.Path.GetFileName(file))
                if not <| System.IO.File.Exists(targetFile)
                then System.IO.File.Copy(file, targetFile)
            copyFile this.ClooPath this.LibraryPath
            copyFile this.MonoOptionsPath this.LibraryPath
             
        member private this.LoadCompiledModule (module_path : string) : unit =
            let compiledassembly = Assembly.LoadFile(module_path)
            let compiled_module = compiledassembly.GetType(this.LibraryName)
            this.LibraryInstance <- Activator.CreateInstance(compiled_module, this.LibraryArgs)
            let compiled_methods = Array.toList <| compiled_module.GetMethods()
            ignore <| List.map (fun (method : MethodInfo) -> do
                        this.AddCompiledFunction method.Name method) compiled_methods
        
        
        member private this.WrapInClass (str: string) = 
            String.Format("public class {0} {{{1}}}", 
                          this.LibraryName, 
                          str)
                          
        member private this.CompileCSharpModule filepath outpath =
            let arguments = String.Format("{3} -target:library -out:{0} -r:{1} -r:{2} /unsafe", 
                                          outpath, this.ClooPath, this.MonoOptionsPath, filepath)
            RunProgram "csc" arguments
            
        member private this.CompileFutharkModule filepath (opencl : bool) =
            let compiler = if opencl then "futhark-csopencl" else "futhark-cs"
            RunProgram compiler (String.Format("--library {0}", filepath))
            
        member private this.AddCompiledFunction name method =
            this.CompiledFunctions <- Map.add name method this.CompiledFunctions
        
        member private this.ConcatenateSources : string = 
            let WrapFunctions (functions : string list) =
                let moduleContents = List.append ["open FSharkPrelude"] functions
                let indent str = "    " + str
                let moduleContents' = String.concat "\n" <| List.map indent moduleContents
                in String.Format("module {0} =\n"+
                                 "{1}\n", this.LibraryName, moduleContents')
                                 
            let imports = String.concat "\n" this.ImportFiles
            let functions = (List.concat << List.map snd << Map.toList) this.InputFunctions
            let functions' = WrapFunctions functions
            let top = "namespace FShark"
            in String.concat "\n" [|top;imports;functions'|]
            
        member private this.GetPathWithSuffix root libName (suffix : string) : string =
            String.Format("{0}/{1}.{2}", root, libName, suffix)
            
        member private this.GetPathWithoutSuffix : string =
            String.Format("{0}/{1}", this.LibraryPath, this.LibraryName)
        
        member this.FSharkArrayToFlatArray (arr : FSharkArray<'a>) =
            if not <| this.IsCompiled
            then failwith "Module needs to be compiled first"
            let tp = arr.GetType
            let (data, dims) = FSharkArrayToFlatArray arr
            in data
            
            
            
            
        member this.CompileAndLoadFSharpModule (rootEntity : FSharpImplementationFileDeclaration) (filepath : string) : unit =
            let decls = FSharkCompiler.FSharkFromFSharpImplementationFileDeclaration(rootEntity)
            let futharkSrc = FutharkWriter.FSharkDeclsToFuthark decls this.Unsafe
            let futharkFilepath = this.GetPathWithSuffix this.LibraryPath this.LibraryName "fut"
            this.WriteSourceToPath futharkSrc futharkFilepath
            let futharkCSPath = System.IO.Path.ChangeExtension(futharkFilepath, "cs")
            let futharkDLLPath = System.IO.Path.ChangeExtension(futharkFilepath, "dll")
            
            COMPILE_SUCCESS(this.CompileFutharkModule futharkFilepath this.OpenCL)
            this.CompileAndLoadCSModule futharkCSPath futharkDLLPath
        end
            
        
    // what about library functions
