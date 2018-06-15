namespace FShark.Library

module ObjectWrappers =
    let CreateInt8Array (data : 'b []) (lens : int64 []) : obj =
                let data8 = Array.map unbox<int8> data
                in (data8, lens) :> obj
    
    let CreateInt16Array (data : 'b []) (lens : int64 []) : obj =
                let data16 = Array.map unbox<int16> data
                in (data16, lens) :> obj
                
    let CreateInt32Array (data : 'b []) (lens : int64 []) : obj =
                let data32 = Array.map unbox<int32> data
                in (data32, lens) :> obj
    
    let CreateInt64Array (data : 'b []) (lens : int64 []) : obj =
                let data64 = Array.map unbox<int64> data
                in (data64, lens) :> obj
                
    let CreateUInt8Array (data : 'b []) (lens : int64 []) : obj =
                let data8 = Array.map unbox<uint8> data
                in (data8, lens) :> obj
    
    let CreateUInt16Array (data : 'b []) (lens : int64 []) : obj =
                let data16 = Array.map unbox<uint16> data
                in (data16, lens) :> obj
                
    let CreateUInt32Array (data : 'b []) (lens : int64 []) : obj =
                let data32 = Array.map unbox<uint32> data
                in (data32, lens) :> obj
    
    let CreateUInt64Array (data : 'b []) (lens : int64 []) : obj =
                let data64 = Array.map unbox<uint64> data
                in (data64, lens) :> obj
    let CreateF32Array (data : 'b []) (lens : int64 []) : obj =
                let data64 = Array.map unbox<single> data
                in (data64, lens) :> obj
                
    let CreateF64Array (data : 'b []) (lens : int64 []) : obj =
                let data64 = Array.map unbox<double> data
                in (data64, lens) :> obj
                
    let CreateBoolArray (data : 'b []) (lens : int64 []) : obj =
                let databool = Array.map unbox<bool> data
                in (databool, lens) :> obj
