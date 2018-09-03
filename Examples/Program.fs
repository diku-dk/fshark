module NBodyExample
open FShark.Main.FSharkMain
open NBodyExample
open LocVolCalibExample
open System.Diagnostics

[<EntryPoint>]
let main argv =
    let nbodyTest = 
        let wrapper = 
            new FSharkMain(
                libName="Matrix",
                tmpRoot="/home/mikkel/FShark",
                preludePath="/home/mikkel/Documents/fshark/FSharkPrelude/bin/Debug/FSharkPrelude.dll",
                openCL=true,
                unsafe=true,
                debug=true,
                LibraryArgs=[|"-t:/home/mikkel/somebenchmarkresults"; "-r=10"|]
                )
                
        wrapper.AddSourceFile "../../srcs/matrix.fs"
        wrapper.CompileAndLoad
    nbodyTest
    0
    
    (*
    let compiledLocVolSmall =
        let clock = new Stopwatch()
        clock.Start()
        let locvol = new LocVolCalibExample([||])
        clock.Stop()
        printfn "Opening class from assembly took %i ms" (FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks)
        
        let foo = fun i ->
            clock.Restart()
            ignore <| locvol.main(16,32,256,256,0.03f,5.0f,0.2f,0.6f,0.5f)
            clock.Stop()
            FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks
        let runtime_list = List.map foo [0..10]
        let sum = List.sum runtime_list
        let average = sum / int64 runtime_list.Length
        printfn "Running locvol small from assembly took %i ms" average
        0
        
    let compiledLocVolMedium =
        let clock = new Stopwatch()
        clock.Start()
        let locvol = new LocVolCalibExample([||])
        clock.Stop()
        printfn "Opening class from assembly took %i ms" (FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks)
        
        let foo = fun i ->
            clock.Restart()
            ignore <| locvol.main(128,256,32,64,0.03f,5.0f,0.2f,0.6f,0.5f)
            clock.Stop()
            FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks
        let runtime_list = List.map foo [0..10]
        let sum = List.sum runtime_list
        let average = sum / int64 runtime_list.Length
        printfn "Running locvol medium from assembly took %i ms" average
        0
        
    let compiledLocVolLarge =
        let clock = new Stopwatch()
        clock.Start()
        let locvol = new LocVolCalibExample([||])
        clock.Stop()
        printfn "Opening class from assembly took %i ms" (FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks)
        
        let foo = fun i ->
            clock.Restart()
            ignore <| locvol.main(256,256,256,64,0.03f,5.0f,0.2f,0.6f,0.5f)
            clock.Stop()
            FShark.Library.Utils.TicksToMicroseconds clock.ElapsedTicks
        let runtime_list = List.map foo [0..1]
        let sum = List.sum runtime_list
        let average = sum / int64 runtime_list.Length
        printfn "Running locvol large from assembly took %i ms" average
        0
    0
    *)
