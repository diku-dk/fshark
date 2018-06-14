namespace FShark

module Program =
    open FShark
    open FShark.Main.FSharkMain
    open FShark.Library.FSharkArrays
    
    let getGenericFshark = 
        new FSharkMain(
        "Internal",
        "/home/mikkel/FShark",
        "/home/mikkel/Cloo.clSharp.dll",
        "/home/mikkel/Mono.Options.dll",
        "/home/mikkel/Documents/FShark/FSharkPrelude/bin/Debug/FSharkPrelude.dll",
         true,
         false
        )
        
    let nbodyTest =
        let fshark = getGenericFshark
        fshark.AddSourceFile "Vec3.fs"
        fshark.AddSourceFile "nbody.fs"
        fshark.CompileAndLoad
         
    let compileTest =
        let fshark = getGenericFshark
        fshark.AddSourceFile "Basic.fs"
        fshark.AddImportFile "SOACS.fs"
        fshark.CompileAndLoad
        fshark
        
    (*
    let arrayTest =
        let fsharkarras = ArrayTests.flattened
        let dims = ArrayTests.dims
        printfn "%A %A" fsharkarras dims
    *)
    
    let main _ =
        nbodyTest
        0
