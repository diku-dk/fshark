namespace FShark.Library

module Utils =
    open System
    open System.IO
    open System.Diagnostics
    open Microsoft.FSharp.Compiler.SourceCodeServices

    let COMPILE_SUCCESS (success : bool, err_string : string) : unit =
        if not <| success then failwith err_string
        else ()
 
    let CreateTempLibDir (rootDir : string) : string =
        let rand = (new System.Random()).Next(10000000,99999999).ToString()
        let path = "tmp"+rand
        let combinedPath = String.Format("{0}/{1}", rootDir, path)
        let _ = System.IO.Directory.CreateDirectory(combinedPath)
        in combinedPath
            
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then Some <| s.Substring(p.Length)
        else None
            
    let rec RemovePrefacingUnderscore (s : string) : string =
        match s with  
        | Prefix "_" rest -> RemovePrefacingUnderscore rest
        | _ -> s
        
    let CompilePanic (errors : FSharpErrorInfo array) : unit =
        ignore <| Array.map (printfn "%s" << string) errors
        exit 1
        

    let RunProgram program arguments : (bool * string) =
        let p = new Process();
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.RedirectStandardError <- true
        p.StartInfo.FileName <- program
        p.StartInfo.Arguments <- arguments
        ignore <| p.Start();
        
        // Synchronously read the standard output of the spawned process. 
        let errs = p.StandardError.ReadToEnd()
        let output = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        (p.ExitCode = 0, errs + output)
            
    
    let rec GetBottomType (variable : obj) : string =
        let tp = variable.GetType()
        if tp.IsArray then
            let variable' = (variable :?> 'a [])
            in GetBottomType <| Array.head variable'
        else variable.GetType().ToString()
