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
open FShark.Library.ObjectWrappers


module FSharkMain =
    exception Error of string
    type FSharkMain = class
        val mutable InputFunctions : Map<string,string list>
        val mutable CompiledFunctions : Map<string,MethodInfo>
        val mutable IsCompiled : bool
        val mutable OpenCL : bool
        val mutable LibraryName : string
        val mutable LibraryRoot : string
        val mutable LibraryPath : string
        val mutable PreludePath : string
        val mutable LibraryInstance : obj
        val mutable LibraryArgs : string array
        val mutable ImportFiles : string list
        val mutable Unsafe : bool
        val mutable Debug : bool
        val mutable MONO_PATH : string
        abstract member AddSourceFile : string -> unit
        abstract member AddImportFile : string -> unit
        
        abstract member CompileAndLoad : unit
        abstract member CompileFunctions : unit
        
        abstract member GetCompiledFunction  : string -> MethodInfo
        
        new ((libName:string),
             (tmpRoot : string),
             (preludePath : string),
             (openCL : bool),
             (unsafe : bool),
             (debug : bool)) =
            let mono_path = Environment.GetEnvironmentVariable("MONO_PATH")
            if mono_path = "" then failwith "Could not find environment variable MONO_PATH"
            { InputFunctions = Map.empty
            ; CompiledFunctions = Map.empty
            ; IsCompiled = false
            ; LibraryName = libName
            ; LibraryRoot = tmpRoot
            ; LibraryPath = CreateTempLibDir tmpRoot
            ; PreludePath = preludePath
            ; LibraryInstance = null
            ; LibraryArgs = Array.empty
            ; ImportFiles = []
            ; OpenCL = openCL
            ; Unsafe = unsafe
            ; Debug = debug
            ; MONO_PATH = mono_path
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
            let pipelineWatch = new Stopwatch()
            pipelineWatch.Start()
            this.LibraryPath <- CreateTempLibDir this.LibraryRoot
            let srcs = this.ConcatenateSources
            let watch = new Stopwatch()
            watch.Start()
            let parsedFile = FSharkParser.ParseAndCheckSingleFile(srcs, this.PreludePath)
            watch.Stop()
            if this.Debug then 
                printfn "FShark parsing took %i ms" <| TicksToMicroseconds watch.ElapsedTicks
            
            if not <| Array.isEmpty parsedFile.Errors then CompilePanic parsedFile.Errors            
            
            watch.Restart()
            let decls = FSharkCompiler.FSharkFromFSharpResults(parsedFile)
            watch.Stop()
            if this.Debug then 
                printfn "FSharpDecls to FSharkIL took %i ms" <| TicksToMicroseconds watch.ElapsedTicks
                
            watch.Restart()
            let futharkSrc = FutharkWriter.FSharkDeclsToFuthark decls this.Unsafe
            watch.Stop()
            if this.Debug then 
                printfn "FSharkIL to Futhark source code took %i ms" <| TicksToMicroseconds watch.ElapsedTicks
            
            
            let futharkPath = this.GetPathWithSuffix this.LibraryPath this.LibraryName "fut"
            let futharkOutPath = this.GetPathWithoutSuffix
            let futharkCSPath = System.IO.Path.ChangeExtension(futharkOutPath, "cs")
            let futharkDLLPath = System.IO.Path.ChangeExtension(futharkOutPath, "dll")
            this.WriteSourceToPath futharkSrc futharkPath
            
            watch.Restart()
            COMPILE_SUCCESS(this.CompileFutharkModule futharkPath this.OpenCL)
            watch.Stop()
            if this.Debug then 
                printfn "Compiling the Futhark module into .cs source code took %i ms" <| TicksToMicroseconds watch.ElapsedTicks
                
            this.CompileAndLoadCSModule futharkCSPath futharkDLLPath
            
            pipelineWatch.Stop()
            if this.Debug then 
                printfn "The entire FShark compilation pipeline took %i ms" <| TicksToMicroseconds pipelineWatch.ElapsedTicks
        
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
            
            
        member this.PrepareFSharkInput (variable : obj) : obj =
            let tp = variable.GetType()
            if tp.IsArray then 
                let (data, lens) = ArrayToFlatArray (variable :?> System.Array)
                match GetBottomType (data.[0]) with
                | "System.Int8" -> 
                    CreateInt8Array data lens
                | "System.Int16" -> 
                    CreateInt16Array data lens
                | "System.Int32" -> 
                    CreateInt32Array data lens
                | "System.Int64" -> 
                    CreateInt64Array data lens
                | "System.UInt8" -> 
                    CreateUInt8Array data lens
                | "System.UInt16" -> 
                    CreateUInt16Array data lens
                | "System.UInt32" -> 
                    CreateUInt32Array data lens
                | "System.UInt64" -> 
                    CreateUInt64Array data lens
                | "System.Single" -> 
                    CreateF32Array data lens
                | "System.Double" -> 
                    CreateF64Array data lens
                | "System.Boolean" -> 
                    CreateBoolArray data lens
                | what -> failwithf "%s" what
            else    
                variable
            
        member this.PrepareFSharkOutput (variable : obj) : obj =
            if FSharpType.IsTuple <| variable.GetType()
            then
                match variable with 
                | :? (int8 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (int16 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (int [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (int64 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (uint8 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (uint16 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (uint32 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (uint64 [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (single [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (double [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
                | :? (bool [] * int64[]) as someTuple ->
                    RestoreFlatArray someTuple
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
            
        member this.InvokeFunction(str : string, [<ParamArray>] parameters : Object[]) =
            let parameters' = (Array.map this.PrepareFSharkInput) parameters
            let (method : MethodInfo) = this.GetCompiledFunction str
            let watch = new Stopwatch()
            if this.Debug then 
                let foo = fun i ->
                    watch.Restart()
                    ignore <| method.Invoke(this.LibraryInstance, parameters')
                    watch.Stop()
                    TicksToMicroseconds watch.ElapsedTicks
                    
                // if OpenCL we repeat the test to ensure warm cache
                let iterations = if this.OpenCL then 10 else 1
                let runtime_list = List.map foo [0..iterations]
                let sum = List.sum runtime_list
                let average = sum / int64 runtime_list.Length
                printfn "Average invokation time was %i ms" average
                
            watch.Restart()
            let result = method.Invoke(this.LibraryInstance, parameters')
            watch.Stop()
            if this.Debug then 
                printfn "Invoking %s took %i ms" str (TicksToMicroseconds watch.ElapsedTicks)
            let result' = this.PrepareFSharkOutput result
            in result'
            
        member private this.CompileCSModule (sourcePath : string) (targetPath : string) : unit =
            COMPILE_SUCCESS(this.CompileCSharpModule sourcePath targetPath)
        
        member private this.CompileAndLoadCSModule (sourcePath : string) (targetPath : string) : unit =
            let cs_watch = new Stopwatch()
                
            cs_watch.Start()
            this.CompileCSModule sourcePath targetPath
            cs_watch.Stop()
            if this.Debug then 
                printfn "Compiling .cs module took %i ms" (TicksToMicroseconds cs_watch.ElapsedTicks)
                
            this.IsCompiled <- true
            cs_watch.Restart()
            this.LoadCompiledModule(targetPath) 
            cs_watch.Stop()
            if this.Debug then 
                printfn "Loading compiled .cs assembly using reflection took %i ms" (TicksToMicroseconds cs_watch.ElapsedTicks)
             
        member private this.LoadCompiledModule (module_path : string) : unit =
            let compiledassembly = Assembly.LoadFile(module_path)
            let compiled_module = compiledassembly.GetType(String.concat "." [this.LibraryName; this.LibraryName])
            this.LibraryInstance <- Activator.CreateInstance(compiled_module, this.LibraryArgs)
            let compiled_methods = Array.toList <| compiled_module.GetMethods()
            ignore <| List.map (fun (method : MethodInfo) -> do
                        this.AddCompiledFunction method.Name method) compiled_methods
        
        
        member private this.WrapInClass (str: string) = 
            String.Format("public class {0} {{{1}}}", 
                          this.LibraryName, 
                          str)
                          
        member private this.CompileCSharpModule cspath outpath =
            let arguments = String.Format("{0} -target:library -out:{1} -lib:{2} -r:Mono.Options.dll -r:Cloo.clSharp.dll /unsafe", 
                                          cspath, outpath, this.MONO_PATH)
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
            let functions' = String.concat "\n" functions
            in String.concat "\n" [|imports;functions'|]
            
        member private this.GetPathWithSuffix root libName (suffix : string) : string =
            String.Format("{0}/{1}.{2}", root, libName, suffix)
            
        member private this.GetPathWithoutSuffix : string =
            String.Format("{0}/{1}", this.LibraryPath, this.LibraryName)
        
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
