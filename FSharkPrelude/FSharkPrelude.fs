namespace FSharkPrelude
[<AutoOpen>]
module FSharkPrelude =
    type FSharkEntry() = inherit System.Attribute()
    
    let ArrayIsNotEmpty (xs : 'a []) : bool =
        (not << Array.isEmpty) xs
    
    let rec Zip (a : 'a array)
                (b : 'b array)
                : ('a * 'b) array =
         
        match (a,b) with
        | ([||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                 ) ->
            Array.append [|(a.[0],b.[0])|] <|
                Zip a.[1..] b.[1..] 
        | _ -> failwith "Irregular arrays"

    let rec Zip3  (a : 'a array)
                        (b : 'b array)
                        (c : 'c array)
                        : ('a* 'b* 'c) array =
        match (a,b,c) with
        | ([||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0])|] <|
                Zip3 a.[1..] b.[1..] c.[1..]
        | _ -> failwith "Irregular arrays"

    let rec Zip4 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       : ('a* 'b* 'c* 'd) array =
        match (a,b,c,d) with
        | ([||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0])|] <|
                Zip4 a.[1..] b.[1..] c.[1..] d.[1..]
        | _ -> failwith "Irregular arrays"
        
    let rec Zip5 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       : ('a* 'b* 'c* 'd* 'e) array =
        match (a,b,c,d,e) with
        | ([||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0])|] <|
                Zip5 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..]
        | _ -> failwith "Irregular arrays"

    let rec Zip6 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       : ('a* 'b* 'c* 'd* 'e* 'f) array =
        match (a,b,c,d,e,f) with
        | ([||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                    && Array.length f > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0],f.[0])|] <|
                Zip6 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..] f.[1..]
        | _ -> failwith "Irregular arrays"
        
    let rec Zip7 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       (g : 'g array)
                       : ('a* 'b* 'c* 'd* 'e* 'f* 'g) array =
        match (a,b,c,d,e,f,g) with
        | ([||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                    && Array.length f > 0
                    && Array.length g > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0],f.[0],g.[0])|] <|
                Zip7 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..] f.[1..] g.[1..]
        | _ -> failwith "Irregular arrays"

    let rec Zip8 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       (g : 'g array)
                       (h : 'h array)
                       : ('a* 'b* 'c* 'd* 'e* 'f* 'g* 'h) array =
        match (a,b,c,d,e,f,g,h) with
        | ([||], [||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                    && Array.length f > 0
                    && Array.length g > 0
                    && Array.length h > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0],f.[0],g.[0],h.[0])|] <|
                Zip8 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..] f.[1..] g.[1..] h.[1..]
        | _ -> failwith "Irregular arrays"

    let rec Zip9 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       (g : 'g array)
                       (h : 'h array)
                       (i : 'i array)
                       : ('a* 'b* 'c* 'd* 'e* 'f* 'g* 'h* 'i) array =
        match (a,b,c,d,e,f,g,h,i) with
        | ([||], [||], [||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                    && Array.length f > 0
                    && Array.length g > 0
                    && Array.length h > 0
                    && Array.length i > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0],f.[0],g.[0],h.[0],i.[0])|] <|
                Zip9 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..] f.[1..] g.[1..] h.[1..] i.[1..]
        | _ -> failwith "Irregular arrays"

    let rec Zip10 (a : 'a array)
                        (b : 'b array)
                        (c : 'c array)
                        (d : 'd array)
                        (e : 'e array)
                        (f : 'f array)
                        (g : 'g array)
                        (h : 'h array)
                        (i : 'i array)
                        (j : 'j array)
                        : ('a* 'b* 'c* 'd* 'e* 'f* 'g* 'h* 'i* 'j) array =
        match (a,b,c,d,e,f,g,h,i,j) with
        | ([||], [||], [||], [||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ when (
                    Array.length a > 0
                    && Array.length b > 0
                    && Array.length c > 0
                    && Array.length d > 0
                    && Array.length e > 0
                    && Array.length f > 0
                    && Array.length g > 0
                    && Array.length h > 0
                    && Array.length i > 0
                    && Array.length j > 0
                 ) ->
            Array.append [|(a.[0],b.[0],c.[0],d.[0],e.[0],f.[0],g.[0],h.[0],i.[0],j.[0])|] <|
                Zip10 a.[1..] b.[1..] c.[1..] d.[1..] e.[1..] f.[1..] g.[1..] h.[1..] i.[1..] j.[1..]
        | _ -> failwith "Irregular arrays"

    let  Unzip (xs : ('a * 'b) array)
       : ('a array * 'b array) =
       let fst' (a,_) = a
       let snd' (_,b) = b
       let map = Array.map
       in (map fst' xs, map snd' xs)

    let  Unzip3 (xs : ('a * 'b * 'c) array)
        : ('a array * 'b array * 'c array) =
        let fst' (a,_,_) = a
        let snd' (_,b,_) = b
        let thrd (_,_,c) = c
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs)

    let  Unzip4 (xs : ('a * 'b * 'c * 'd) array)
        : ('a array * 'b array * 'c array * 'd array) =
        let fst' (a,_,_,_) = a
        let snd' (_,b,_,_) = b
        let thrd (_,_,c,_) = c
        let frth (_,_,_,d) = d
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs)

    let  Unzip5 (xs : ('a * 'b * 'c * 'd * 'e) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array) =
        let fst' (a,_,_,_,_) = a
        let snd' (_,b,_,_,_) = b
        let thrd (_,_,c,_,_) = c
        let frth (_,_,_,d,_) = d
        let ffth (_,_,_,_,e) = e
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs)

    let  Unzip6 (xs : ('a * 'b * 'c * 'd * 'e * 'f) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array) =
        let fst' (a,_,_,_,_,_) = a
        let snd' (_,b,_,_,_,_) = b
        let thrd (_,_,c,_,_,_) = c
        let frth (_,_,_,d,_,_) = d
        let ffth (_,_,_,_,e,_) = e
        let sxth (_,_,_,_,_,f) = f
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs, map sxth xs)

    let  Unzip7 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array) =
        let fst' (a,_,_,_,_,_,_) = a
        let snd' (_,b,_,_,_,_,_) = b
        let thrd (_,_,c,_,_,_,_) = c
        let frth (_,_,_,d,_,_,_) = d
        let ffth (_,_,_,_,e,_,_) = e
        let sxth (_,_,_,_,_,f,_) = f
        let svth (_,_,_,_,_,_,g) = g
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs, map sxth xs, map svth xs)

    let  Unzip8 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array) =
        let fst' (a,_,_,_,_,_,_,_) = a
        let snd' (_,b,_,_,_,_,_,_) = b
        let thrd (_,_,c,_,_,_,_,_) = c
        let frth (_,_,_,d,_,_,_,_) = d
        let ffth (_,_,_,_,e,_,_,_) = e
        let sxth (_,_,_,_,_,f,_,_) = f
        let svth (_,_,_,_,_,_,g,_) = g
        let eght (_,_,_,_,_,_,_,h) = h
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs, map sxth xs, map svth xs, map eght xs)

    let  Unzip9 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array * 'i array) =
        let fst' (a,_,_,_,_,_,_,_,_) = a
        let snd' (_,b,_,_,_,_,_,_,_) = b
        let thrd (_,_,c,_,_,_,_,_,_) = c
        let frth (_,_,_,d,_,_,_,_,_) = d
        let ffth (_,_,_,_,e,_,_,_,_) = e
        let sxth (_,_,_,_,_,f,_,_,_) = f
        let svth (_,_,_,_,_,_,g,_,_) = g
        let eght (_,_,_,_,_,_,_,h,_) = h
        let nnth (_,_,_,_,_,_,_,_,i) = i
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs, map sxth xs, map svth xs, map eght xs, map nnth xs)

    let  Unzip10 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array * 'i array * 'j array) =
        let fst' (a,_,_,_,_,_,_,_,_,_) = a
        let snd' (_,b,_,_,_,_,_,_,_,_) = b
        let thrd (_,_,c,_,_,_,_,_,_,_) = c
        let frth (_,_,_,d,_,_,_,_,_,_) = d
        let ffth (_,_,_,_,e,_,_,_,_,_) = e
        let sxth (_,_,_,_,_,f,_,_,_,_) = f
        let svth (_,_,_,_,_,_,g,_,_,_) = g
        let eght (_,_,_,_,_,_,_,h,_,_) = h
        let nnth (_,_,_,_,_,_,_,_,i,_) = i
        let tnth (_,_,_,_,_,_,_,_,_,j) = j
        let map = Array.map
        in (map fst' xs, map snd' xs, map thrd xs, map frth xs, map ffth xs, map sxth xs, map svth xs, map eght xs, map nnth xs, map tnth xs)

    let Map f aa =
        Array.map f aa

    let Map2 f aa bb =
        let curry f (a,b) = f a b 
        let xs = Zip aa bb
        in Array.map (curry f) xs

    let Map3 f aa bb cc =
        let curry f (a,b,c) = f a b c
        let xs = Zip3 aa bb cc
        in Array.map (curry f) xs

    let Map4 f aa bb cc dd =
        let curry f (a,b,c,d) = f a b c d
        let xs = Zip4 aa bb cc dd
        in Array.map (curry f) xs

    let Map5 f aa bb cc dd ee =
        let curry f (a,b,c,d,e) = f a b c d e
        let xs = Zip5 aa bb cc dd ee
        in Array.map (curry f) xs

    let Map6 f aa bb cc dd ee ff =
        let curry f (a,b,c,d,e,f') = f a b c d e f'
        let xs = Zip6 aa bb cc dd ee ff
        in Array.map (curry f) xs

    let Map7 f aa bb cc dd ee ff gg =
        let curry f (a,b,c,d,e,f',g) = f a b c d e f' g
        let xs = Zip7 aa bb cc dd ee ff gg
        in Array.map (curry f) xs

    let Map8 f aa bb cc dd ee ff gg hh =
        let curry f (a,b,c,d,e,f',g,h) = f a b c d e f' g h 
        let xs = Zip8 aa bb cc dd ee ff gg hh
        in Array.map (curry f) xs

    let Map9 f aa bb cc dd ee ff gg hh ii =
        let curry f (a,b,c,d,e,f',g,h,i) = f a b c d e f' g h i
        let xs = Zip9 aa bb cc dd ee ff gg hh ii
        in Array.map (curry f) xs

    let Map10 f aa bb cc dd ee ff gg hh ii jj =
        let curry f (a,b,c,d,e,f',g,h,i,j) = f a b c d e f' g h i j
        let xs = Zip10 aa bb cc dd ee ff gg hh ii jj
        in Array.map (curry f) xs

    let Reduce (op: 'a -> 'a -> 'a) (neutral : 'a) (xs : 'a array) =
        let xs' = Array.append [|neutral|] xs
        in Array.reduce op xs'

    let Reduce_Comm (op: 'a -> 'a -> 'a) (neutral : 'a) (xs : 'a array) =
        let xs' = Array.append [|neutral|] xs
        in Array.reduce op xs'

    let Scan (op: 'a -> 'a -> 'a) (neutral : 'a) (xs : 'a array) : 'a array =
        (Array.tail << Array.scan op neutral) xs
        
    let Filter (p: 'a -> bool) (xs : 'a array) =
        Array.filter p xs

    let Partition (p: 'a -> bool) (xs : 'a array) =
        Array.Parallel.partition p xs

    let Partition2 (p1: 'a -> bool) (p2: 'a -> bool) (xs : 'a array) =
        let p' x = if p1 x then 0 else if p2 x then 1 else 2
        let flagged = Array.zip xs <| Array.map p' xs
        let (zeroes,_) = Unzip <| Array.filter (fun (_, i) -> i = 0) flagged
        let (ones,_) = Unzip <| Array.filter (fun (_, i) -> i = 1) flagged
        let (twos,_) = Unzip <| Array.filter (fun (_, i) -> i = 2) flagged
        in (zeroes, ones, twos)

    let All (f: 'a -> bool) (xs : 'a array) : bool =
        Array.forall f xs

    let Any (f: 'a -> bool) (xs : 'a array) : bool =
        Array.exists f xs

    let Scatter (dest : 'a array) (is : int array) (vs : 'a array) : 'a array =
        for (i,v) in Zip is vs do
            dest.[i] <- v
        dest

    let Range (a : int) (b : int) : int array =
        Seq.toArray <| [a..b]

    let Foldl (f : 'a -> 'b -> 'a) (acc: 'a) (bs : 'b array) : 'a =
        Array.fold f acc bs
        
    let Foldr (f : 'a -> 'b -> 'b) (acc: 'b) (bs : 'a array) : 'b =
        Array.foldBack f bs acc

    let Length (xs : 'a array) : int =
        Array.length xs
    
    let Null (xs : 'a array) : bool =
        Array.isEmpty xs
    
    let Head (xs : 'a array) : 'a =
        Array.head xs
              
    let Last (xs : 'a array) : 'a =
        Array.last xs
        
    let Tail (xs : 'a array) : 'a array =
        Array.tail xs
        
    let Init (xs : 'a array) : 'a array =
        Array.take (Array.length xs - 1) xs
        
    let Take (i : int) (xs : 'a array) : 'a array =
        Array.take i xs
        
    let Drop (i : int) (xs : 'a array) : 'a array =
        Array.skip i xs
        
    let Split (i : int) (xs : 'a array) : ('a array * 'a array) =
        (Array.take i xs, Array.skip i xs)
        
    let Split2 (i : int) (j : int) (xs : 'a array) : ('a array * 'a array * 'a array) =
        (Array.take i xs, Array.skip i <| Array.take j xs, Array.skip j xs)
        
    let Reverse (xs : 'a array) : 'a array =
        Array.rev xs
        
    let Concat (xs : 'a array) (ys : 'a array) : 'a array =
        Array.concat [xs; ys]
        
    let Rotate (i : int) (xs : 'a array) : 'a array =
        let len = Array.length xs
        if abs i > len then xs else 
        if i > 0 then
            Array.concat [Array.skip i xs; Array.take i xs]
        else 
            Array.concat [Array.skip (len-abs i) xs; Array.take (len-abs i) xs]
        
    let Update (xs : 'a array) (i : int) (x : 'a) : 'a array =
        Array.set xs i x
        xs
        
    let Iota (n : int) : int array =
        Seq.toArray <| [0..n-1]
    
    let Replicate (n : int) (x : 'a) : 'a array =
        List.toArray <| List.replicate n x
        
    let Copy (xs : 'a array) : 'a array =
        Array.copy xs
        
    let Flatten xs : 'a array =
        Array.concat xs
            
    let Flatten_3D xs : 'a array =
        Flatten <| Flatten xs
        
    let Flatten_4D xs : 'a array =
        Flatten <| Flatten_3D xs
        
    let rec Transpose (xs : 'a array array) : 'a array array =
        if Array.length xs.[0] = 0
        then Array.empty
        else 
            let heads = Array.map Array.head xs
            let tails = Array.map Array.tail xs
            in Array.append [|heads|] <| Transpose tails 


    (* TODO
    
let unflatten 't (n: i32) (m: i32) (xs: []t): [n][m]t =
  intrinsics.unflatten (n, m, xs)

-- | Splits the outer dimension of an array in three.
let unflatten_3d 't (n: i32) (m: i32) (l: i32) (xs: []t): [n][m][l]t =
  unflatten n m (unflatten (n*m) l xs)

-- | Splits the outer dimension of an array in four.
let unflatten_4d 't (n: i32) (m: i32) (l: i32) (k: i32) (xs: []t): [n][m][l][k]t =
  unflatten n m (unflatten_3d (n*m) l k xs)

let intersperse [n] 't (x: t) (xs: [n]t): *[]t =
  map (\i -> if i % 2 == 1 && i != 2*n then x
             else unsafe xs[i/2])
      (iota (i32.max (2*n-1) 0))

let intercalate [n] [m] 't (x: [m]t) (xs: [n][m]t): []t =
  unsafe flatten (intersperse x xs)

let transpose [n] [m] 't (a: [n][m]t): [m][n]t =
  intrinsics.transpose a

let steps (start: i32) (num_steps: i32) (step: i32): [num_steps]i32 =
  map (start+) (map (step*) (* (iota num_steps))

let range (start: i32) (end: i32) (step: i32): []i32 =
  let w = (end-start)/step
  in steps start w step

-- | True if all of the input elements are true.  Produces true on an
-- empty array.
let and: []bool -> bool = all id

-- | True if any of the input elements are true.  Produces false on an
-- empty array.
let or: []bool -> bool = any id

let pick [n] 't (flags: [n]bool) (xs: [n]t) (ys: [n]t): *[n]t =
  map3 (\flag x y -> if flag then x else y) flags xs ys

-- | Perform a *sequential* left-fold of an array.
let foldl 'a 'b (f: a -> b -> a) (acc: a) (bs: []b): a =
  loop acc for b in bs do f acc b

-- | Perform a *sequential* right-fold of an array.
let foldr 'a 'b (f: b -> a -> a) (acc: a) (bs: []b): a =
  foldl (flip f) acc (reverse bs)

-- | Create a value for each point in a one-dimensional index space.
let tabulate 'a (n: i32) (f: i32 -> a): *[n]a =
  map1 f (iota n)

-- | Create a value for each point in a two-dimensional index space.
let tabulate_2d 'a (n: i32) (m: i32) (f: i32 -> i32 -> a): *[n][m]a =
  map1 (f >-> tabulate m) (iota n)
     *)


    (*let Sequence (a : int) (step: int) (b : int) : int array =
        Seq.toArray <| [a..step..b]
        *)
        
