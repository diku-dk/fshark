open System
open FShark.Library
open FShark.Main
open FSharkTest.TestBase

type ToTest = SingleTest of string
            | FolderTest of string
            
let showDocumentation : Lazy<unit> =
    lazy (printfn "Here is good help")

[<EntryPoint>]
let main argv =

    let f = LocVolCalib.main
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    //let res = f 16 32 256 256 0.03f 5.0f 0.2f 0.6f 0.5f
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds

    let PreludeDLL = "/home/mikkel/Documents/fshark/FSharkPrelude/bin/Debug/FSharkPrelude.dll"
    let TestDLL = "/home/mikkel/Documents/fshark/bin/Debug/FShark.dll"
    if Array.length argv <> 1
    then ignore <| showDocumentation; 1
    else
    let fshark = new FSharkMain.FSharkMain(
        "Internal",
        "/home/mikkel/FShark",
        "/home/mikkel/Cloo.clSharp.dll",
        "/home/mikkel/Mono.Options.dll",
        "/home/mikkel/Documents/fshark/FSharkPrelude/bin/Debug/FSharkPrelude.dll",
         true,
         true
        )
    let tester = new Tester(PreludeDLL, TestDLL, fshark, true)
    match Array.head argv with 
    | Utils.Prefix "-s:" path -> 
        tester.RunSingleTest path
    | Utils.Prefix "-f:" path -> 
        tester.RunFolderOfTests path
        
    0 // return an integer exit code
