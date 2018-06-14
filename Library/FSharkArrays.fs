namespace FShark.Library

module FSharkArrays =
  open System.Text.RegularExpressions

  type FSharkArray<'a> = Header of System.Int32
                       | Dimension of FSharkArray<'a> list
                       | Bottom of 'a []
              
  let rec FSharkArrayToFlatArray (arr : FSharkArray<'a>) : ('a [] * int64 []) =
     let checkRegularity arrs : unit =
        if Array.length arrs = 0
        then failwith "Empty array"
        
        let head = Array.head arrs
        if not <| Array.forall (fun l -> l = head) arrs
        then failwith "Irregular array"
        
     match arr with
     | Bottom list -> (list, [|int64 <| Array.length list|])
     | Dimension(subarrays) ->
        let a =  List.toArray <| List.map FSharkArrayToFlatArray subarrays
        let (subarrs, lens) = Array.unzip a
        checkRegularity lens
        let subarrs' = Array.concat subarrs
        let lens' = Array.head lens
        let len_subarrs = int64 <| List.length subarrays
        let lens_out = Array.append [|len_subarrs|] lens'
        (subarrs', lens_out)
     | Header _ -> failwith "why this case?"
     
     
  // husk at takke Christopher Pritchard og Abe Mieres fra F#-slacken
  let rec ArrayToFlatArray (array : System.Array) = 
      if array.Length = 0 then failwith "Empty array"
      let array0 = array  |> Seq.cast |> Seq.toArray
      let arrays = array0 |> Array.choose (box >> function
                                            | :? System.Array as xs -> Some xs
                                            | _                     -> None
                                          ) 
      let lengths = Array.map (Seq.cast >> Seq.length) arrays |> Array.distinct
      if   arrays .Length  = 0            then array0 |> Array.map unbox, [|int64 array.Length|]
      elif arrays .Length <> array.Length then failwith "Invalid array"
      elif lengths.Length  > 1            then failwith "Irregular array"
      else
          let a             = Array.map ArrayToFlatArray arrays
          let subarrs, lens = Array.unzip a
          let subarrs'      = Array.concat subarrs
          let lens'         = Array.head lens
          let len_subarrs   = int64 array.Length
          let lens_out      = Array.append [|len_subarrs|] lens'
          subarrs', lens_out  
          
    
  let rec FlatArrayToFSharkArray data (lens : int64 []) : obj =
      match lens.Length with 
      | 1 -> box data
      | _ ->
         let len = Array.head lens
         let lens' = Array.tail lens
         let data' = Array.splitInto (int len) data
         let data'' = Array.map (fun arr -> FlatArrayToFSharkArray arr lens') data'
         in box data''
          
     (*
     match arr with
     | Dimension(subarrays) ->
     | Header _ -> failwith "why this case?"
     *)
