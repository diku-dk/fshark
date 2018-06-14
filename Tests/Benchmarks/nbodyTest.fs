namespace FShark.Tests.Benchmarks

module NBodyTest =
open FShark.Main.FSharkMain

let nbodyTest = do
    let fshark = new FSharkMain(
        "Internal",
        "/home/mikkel/FShark",
        "/home/mikkel/Cloo.clSharp.dll",
        "/home/mikkel/Mono.Options.dll",
        "/home/mikkel/Documents/FShark/FSharkPrelude/bin/Debug/FSharkPrelude.dll",
         true,
         false
        )
    
    in fshark
