module NBodyExample
open FShark.Main.FSharkMain
open NBodyExample
open LocVolCalibExample
open System.Diagnostics

let tupleFromArray (input : float32[]) : (float32[] * int64[]) =
    (input, [|int64 input.Length|])


module nbodyData =
    let xpsIn = [|49.934765f; -19.72055f; -8.485103f; -40.529545f; 36.915504f; 58.12199f; -18.56515f; -16.319696f; -35.320797f; 35.467846f; 22.741138f; -19.066488f; -4.6187997f; -37.741123f; 36.013374f; 49.562435f; -9.462863f; -10.790539f; -40.558056f; 29.8883f; 32.482105f; -20.018032f; -12.006343f; -36.06932f; 36.564384f; 72.98635f; -6.637457f; -5.4672456f; -43.633926f; 54.48485f; 50.216465f; -24.18221f|]
    let ypsIn = [|48.864784f; 21.0447f; 70.43111f; -23.411388f; -13.378705f; 47.11681f; 19.121122f; 68.65275f; -9.581309f; -31.458008f; 16.762173f; 32.53443f; 54.83043f; -23.498236f; -17.063395f; 83.03267f; 25.536043f; 66.63314f; -29.971233f; -20.165321f; 30.910307f; 19.918154f; 50.781525f; -13.634953f; -22.889074f; 36.489113f; 11.754714f; 71.23518f; -22.012821f; -44.066643f; 50.00622f; 1.1176014f|]
    let zpsIn = [|49.504097f; 21.976465f; -66.07767f; -23.874193f; 22.885813f; 49.27831f; 19.856537f; -70.91549f; -15.5530615f; 57.63878f; 47.443493f; 19.284454f; -69.1615f; -29.302645f; 32.41684f; 83.920395f; 36.41799f; -67.998985f; -24.474981f; 45.04451f; 49.05143f; 27.472118f; -48.374542f; -28.24347f; 41.142605f; 44.127728f; 14.4537f; -59.5781f; -7.0467205f; 29.847332f; 49.413647f; 18.39111f|]
    let msIn = [|3.456227f; 63.459526f; 74.34554f; 8.434827f; 16.776989f; 21.63172f; 69.831604f; 66.98223f; 23.477612f; 4.5274367f; 45.534313f; 37.293446f; 52.771004f; 83.92678f; 13.032608f; 18.12446f; 53.132633f; 94.62536f; 6.5728045f; 53.180336f; 91.3407f; 87.52959f; 75.542244f; 35.689384f; 77.35545f; 34.4798f; 20.805649f; 58.3743f; 20.659788f; 82.26547f; 86.262726f; 52.37195f|]
    let xvsIn = [|10.561408f; 6.989762f; 14.306272f; -6.4595428f; -3.9690585f; 9.955754f; 6.6335444f; 13.727621f; -3.0388143f; -7.2828484f; 4.5114946f; 9.998227f; 11.664729f; -6.4404936f; -4.7614217f; 14.676114f; 7.573575f; 13.614646f; -8.00616f; -5.309554f; 7.583363f; 6.346644f; 12.0401945f; -3.9444003f; -5.9291406f; 7.5769863f; 5.286392f; 14.7713995f; -6.265288f; -10.098527f; 10.759963f; 0.40538555f|]
    let yvsIn = [|-10.792668f; 6.5499606f; 1.7235309f; 11.182692f; -10.951718f; -12.281143f; 6.440665f; 3.263243f; 11.202367f; -8.211167f; -6.1207175f; 5.859364f; 0.9826122f; 10.344243f; -10.049282f; -8.760214f; 2.8065314f; 2.204749f; 10.834197f; -7.869626f; -7.9689794f; 6.378469f; 2.8466792f; 10.434348f; -9.471566f; -15.15566f; 2.9850323f; 1.1336936f; 12.4190855f; -12.486015f; -10.805201f; 8.771569f|]
    let zvsIn = [|10.699586f; 7.2992377f; -13.421982f; -6.5872374f; 6.7895308f; 10.4124775f; 6.8886757f; -14.180073f; -4.932819f; 13.343963f; 12.769291f; 5.926348f; -14.713549f; -8.03139f; 9.045693f; 14.83302f; 10.800983f; -13.893719f; -6.5379558f; 11.860275f; 12.034005f; 8.753611f; -11.469504f; -8.17044f; 10.6575f; 9.163149f; 6.5001945f; -12.354176f; -2.0056372f; 6.8399606f; 10.632457f; 6.670974f|]
    let xasIn = [|-0.2020692f; 3.0261776f; -0.1371406f; 0.42419815f; -0.9118709f; -1.5379857f; 2.988307f; 1.625271f; 1.208595f; -0.12642522f; -1.232773f; 1.6313204f; -0.81280035f; 3.5243607f; -0.61353976f; -0.4695076f; 1.3853245f; 0.56374645f; 0.30523086f; -1.4631323f; -3.6304643f; 4.4817963f; 0.74818593f; 1.5595063f; -3.1575167f; -3.1058714f; 6.1454386e-2f; -0.6793159f; 1.3458371f; -4.644851f; -5.0198126f; 2.865766f|]
    let yasIn = [|-0.18428235f; -0.7999116f; -3.710925f; 0.5393226f; 0.8006712f; -0.9758822f; -0.41186926f; -2.7936146f; 1.1207385f; 0.3105037f; 2.6571987e-2f; -1.8151838f; -0.112044565f; 5.315734f; 0.70832497f; -1.0319139f; -1.4187478f; -3.55292f; 0.47446692f; 3.3048942f; -2.5821278f; -0.7540304f; -0.64892244f; 1.77316f; 4.8282127f; -0.73178864f; 0.3914103f; -3.241285f; 1.3664191f; 6.0297318f; -4.7489667f; 2.5242555f|]
    let zasIn = [|-0.13120669f; -1.3126944f; 3.796874f; 0.31401542f; 0.2444f; -0.7344652f; -0.9475892f; 4.267117f; 0.7841558f; -0.2921983f; -2.692973f; -0.52129644f; 3.5451076f; 3.9011729f; -0.11806177f; -1.0167954f; -3.0760522f; 5.51218f; 0.23335022f; -2.3880641f; -4.8232713f; -3.3089616f; 1.946604f; 1.7561425f; -2.4603796f; -0.6773815f; 0.19268043f; 1.9731628f; 0.22117926f; -0.19813985f; -3.2173421f; -0.771268f|]
    let nbodyInput = 
                [|
                    1000;
                    50.0f;
                    0.1f;
                    xpsIn;
                    ypsIn;
                    zpsIn;
                    msIn;
                    xvsIn;
                    yvsIn;
                    zvsIn;
                    xasIn;
                    yasIn;
                    zasIn
                |] : obj array
                
                
module LocVolCalibData =
    let locVolSmall = 
                [|
                    16;
                    32;
                    256;
                    256;
                    0.03f;
                    5.0f;
                    0.2f;
                    0.6f;
                    0.5f;
                |] : obj array
            
    let locVolMedium = 
                [|
                    128;
                    256;
                    32;
                    64
                    0.03f;
                    5.0f;
                    0.2f;
                    0.6f;
                    0.5f;
                |] : obj array
                
    let locVolLarge = 
                [|
                    256;
                    256;
                    256;
                    64;
                    0.03f;
                    5.0f;
                    0.2f;
                    0.6f;
                    0.5f;
                |] : obj array


[<EntryPoint>]
let main argv =
    let nbodyTest = 
        let wrapper = 
            new FSharkMain(
                libName="LocVolCalibExample",
                tmpRoot="/home/mikkel/FShark",
                preludePath="/home/mikkel/Documents/fshark/FSharkPrelude/bin/Debug/FSharkPrelude.dll",
                openCL=true,
                unsafe=true,
                debug=true,
                LibraryArgs=[|"-t:/home/mikkel/somebenchmarkresults"; "-r=10"|]
                )
                
        wrapper.AddSourceFile "../../srcs/LocVolCalib.fs"
        wrapper.CompileAndLoad
        let locVolCalib_results =
            wrapper.InvokeFunction("main", 128, 256, 32, 64, 0.03f, 5.0f, 0.2f, 0.6f, 0.5f) :?> single array
        printfn "%A" locVolCalib_results
        
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
