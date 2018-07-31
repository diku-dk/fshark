namespace FSharkTest

open System.IO
open System.Reflection
open System
open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices

    
module TestBase =
    open System.Collections.Generic
    open System.Collections.Generic
    open FShark.Library
    open FShark
    open FShark.Main.FSharkMain

    type TestDict = {inputName: string
                    ;outputName: string
                    ;entryName: string
                    }
    
    let CheckTestExists (filepath : string) : unit =
        if System.IO.File.Exists(filepath) 
        then 
            ()
        else 
            printfn "File %s doesn't exists" filepath
            exit 1
    
    let GetFilesFromFolder (dir : string) : string list =
        let files = Seq.toList <| Directory.GetFiles(dir, "*.fs", SearchOption.AllDirectories)
        in files
        
    
    type Tester = class 
        val mutable Checker : FSharpChecker
        val mutable PreludeLib : string
        val mutable FSharkLib : string
        val mutable fsharkMain : FSharkMain
        val mutable benchmark : bool
        abstract member RunFolderOfTests : string -> unit
        abstract member RunSingleTest : string -> unit
        
        new (preludePath, libPath, fshark, benchmark) = 
            let checker = FSharpChecker.Create(keepAssemblyContents=true)
            { Checker=checker
            ; FSharkLib=libPath
            ; PreludeLib=preludePath
            ; fsharkMain=fshark
            ; benchmark=benchmark
            }
            
            
                        
        member this.getElemWithAttribute (elems : FSharpImplementationFileDeclaration list) 
                                         (attrName : string) (testname : string) : (string * FSharpImplementationFileDeclaration) =
            let maybeElem maybe elem =
                match elem with 
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) ->
                    match maybe with
                    | Some _ -> maybe
                    | None -> if List.exists (fun (attr : FSharpAttribute) -> attr.AttributeType.DisplayName = attrName) 
                                 (Seq.toList <| v.Attributes)
                              then Some (v.DisplayName, elem)
                              else None
                | _ -> None
                              
            let res = List.fold maybeElem Option.None elems
            in match res with
            | Some elem -> elem
            | None -> failwithf "%s doesn't contain attribute %s" testname attrName
        
        member this.CompileUnitTest filepath targetpath : unit =
            CheckTestExists filepath
            if File.Exists targetpath then ()
            let errs, exitCode = 
                Async.RunSynchronously
                <| this.Checker.Compile([|"fsharpc";  
                                          "-r"; this.FSharkLib; 
                                          "-r"; this.PreludeLib; 
                                          "-o"; targetpath; 
                                          "-a"; filepath |])
            match exitCode with
            | n when n <> 0 -> Utils.CompilePanic errs
            | 0 -> ()
            
            
        member this.LoadUnitTestAssembly filepath targetpath : (TestDict * FSharpImplementationFileDeclaration * Assembly) = 
            let filecontents = File.ReadAllText filepath
            let refFlag = [|"-r:"+this.FSharkLib
                          ; "-r:"+this.PreludeLib|]
            let (projOptions, _) = 
                Async.RunSynchronously <|
                this.Checker.GetProjectOptionsFromScript(filepath, filecontents,otherFlags=refFlag)
            
            let result = 
                Async.RunSynchronously <|
                this.Checker.ParseAndCheckProject(projOptions) 
            let rootEntity = result.AssemblyContents.ImplementationFiles.Head.Declarations.Head
            match rootEntity with
            | Entity(_, result') ->
                let ((inputName,_),(outputName,_),(entryName,testFunction)) =
                        ( this.getElemWithAttribute result' "FSharkInput" filepath
                        , this.getElemWithAttribute result' "FSharkOutput" filepath
                        , this.getElemWithAttribute result' "FSharkEntry" filepath
                        )
                let testDict = {inputName=inputName; outputName=outputName;entryName=entryName}
                let assembly = Assembly.LoadFile targetpath
                in (testDict, rootEntity, assembly)
            | _ -> failwith "uh oh"
            
            
            
        member this.RunUnitTest (filepath : string) : bool =
            let targetpath = Path.ChangeExtension(filepath, ".dll")
            let libraryName = Path.GetFileNameWithoutExtension(filepath)
            this.CompileUnitTest filepath targetpath
            let (testDict, testEntity, assembly) = this.LoadUnitTestAssembly filepath targetpath
            let testTopModule = Seq.head <| assembly.Modules 
            let testModule = (Seq.head (Seq.head (Seq.head <| testTopModule.Assembly.ExportedTypes).Assembly.ExportedTypes).Assembly.ExportedTypes)
            
            let testInput  = (testModule.GetProperty testDict.inputName).GetValue(0) :?> obj array
            let testOutput = (testModule.GetProperty testDict.outputName).GetValue(0)
            let testFun  = testModule.GetMethod testDict.entryName
            
            this.fsharkMain.LibraryName <- "tmp_"+testModule.Name
            this.fsharkMain.CompileAndLoadFSharpModule testEntity filepath
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let fsharkTestResult = 
                try 
                    Some <| this.fsharkMain.InvokeFunction(testDict.entryName, testInput)
                with 
                    | ex -> printfn "%s" ex.Message; None
            stopWatch.Stop()
            printfn "FShark took %d microseconds." <| FShark.Library.Utils.TicksToMicroseconds stopWatch.ElapsedTicks
            match fsharkTestResult with
            | None -> false
            | Some fsharkTestResult' ->
            
            let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()
            let nativeTestResult = testFun.Invoke(0, testInput)
            stopWatch2.Stop()
            printfn "Native took %d microseconds." <| FShark.Library.Utils.TicksToMicroseconds stopWatch2.ElapsedTicks
            let testOK = 
                fsharkTestResult' = nativeTestResult &&
                testOutput = nativeTestResult
            
            in testOK
        
        // this is an entry function
        default this.RunFolderOfTests (folderpath : string) : unit =
            let tests = GetFilesFromFolder folderpath
            let num_tests = List.length tests
            let tests' = List.zip tests <| Seq.toList [| 1 .. num_tests |]
            let foldLoop acc ((test_path : string), test_num) =
                let test_path' = test_path.Substring(folderpath.Length+1)
                let tests_left = num_tests - test_num
                let acc' = acc+1
                let failed = test_num - (acc')
                match this.RunUnitTest test_path with
                | true -> 
                    printfn "Unit test %s ran succesfully (%d successful, %d failed). %d tests remaining." 
                             test_path' acc' failed tests_left 
                    acc'
                    
                | false ->
                    printfn "Unit test %s failed (%d successful, %d failed). %d tests remaining." 
                            test_path' acc failed tests_left 
                    acc
                    
            ignore <| List.fold foldLoop 0 tests'
        
        // this is an entry function
        default this.RunSingleTest (filepath : string) : unit =
            if this.RunUnitTest filepath
            then printfn "Unit test %s ran succesfully." filepath
            else printfn "Unit test %s failed." filepath
    end
