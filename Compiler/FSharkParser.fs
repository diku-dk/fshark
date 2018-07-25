namespace FShark.Compiler

module FSharkParser = 
    open System
    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FShark.IL

    let ParseAndCheckSingleFile(fsharpString : string, fsharkLibPath : string) : FSharpCheckProjectResults =
        let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")
        File.WriteAllText(file, fsharpString)
        // Get context representing a stand-alone (script) file
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let refFlag = [|"-r:"+fsharkLibPath|]
        let (projOptions,_) = 
            checker.GetProjectOptionsFromScript(file, fsharpString, otherFlags=refFlag)
            |> Async.RunSynchronously
        let result = checker.ParseAndCheckProject(projOptions) 
                     |> Async.RunSynchronously
        in result

