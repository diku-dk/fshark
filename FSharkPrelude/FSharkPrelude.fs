namespace FSharkPrelude
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open System.Reflection

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
        | _ ->
        let len1 = a.Length
        if len1 <> b.Length then
            failwith "Irregular arrays"
        else
        let res = Array.zeroCreate len1
        for i = 0 to res.Length-1 do
            res.[i] <- (a.[i]
                       ,b.[i])
        res

    let rec Zip3  (a : 'a array)
                        (b : 'b array)
                        (c : 'c array)
                        : ('a* 'b* 'c) array =
        match (a,b,c) with
        | ([||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i])
        res

    let rec Zip4 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       : ('a* 'b* 'c* 'd) array =
        match (a,b,c,d) with
        | ([||], [||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i])
        res
        
    let rec Zip5 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       : ('a* 'b* 'c* 'd* 'e) array =
        match (a,b,c,d,e) with
        | ([||], [||], [||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i])
        res
        
    let rec Zip6 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       : ('a* 'b* 'c* 'd* 'e* 'f) array =
        match (a,b,c,d,e,f) with
        | ([||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
           && lenA <> f.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i]
                       ,f.[i])
        res
        
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
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
           && lenA <> f.Length
           && lenA <> g.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i]
                       ,f.[i]
                       ,g.[i])
        res

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
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
           && lenA <> f.Length
           && lenA <> g.Length
           && lenA <> h.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i]
                       ,f.[i]
                       ,g.[i]
                       ,h.[i])
        res

    let rec Zip9 (a : 'a array)
                       (b : 'b array)
                       (c : 'c array)
                       (d : 'd array)
                       (e : 'e array)
                       (f : 'f array)
                       (g : 'g array)
                       (h : 'h array)
                       (i' : 'i array)
                       : ('a* 'b* 'c* 'd* 'e* 'f* 'g* 'h* 'i) array =
        match (a,b,c,d,e,f,g,h,i') with
        | ([||], [||], [||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
           && lenA <> f.Length
           && lenA <> g.Length
           && lenA <> h.Length
           && lenA <> i'.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i]
                       ,f.[i]
                       ,g.[i]
                       ,h.[i]
                       ,i'.[i])
        res

    let rec Zip10 (a : 'a array)
                        (b : 'b array)
                        (c : 'c array)
                        (d : 'd array)
                        (e : 'e array)
                        (f : 'f array)
                        (g : 'g array)
                        (h : 'h array)
                        (i' : 'i array)
                        (j : 'j array)
                        : ('a* 'b* 'c* 'd* 'e* 'f* 'g* 'h* 'i* 'j) array =
        match (a,b,c,d,e,f,g,h,i',j) with
        | ([||], [||], [||], [||], [||], [||], [||], [||], [||], [||]) -> Array.empty
        | _ ->
        let lenA = a.Length
        if    lenA <> b.Length
           && lenA <> c.Length
           && lenA <> d.Length
           && lenA <> e.Length
           && lenA <> f.Length
           && lenA <> g.Length
           && lenA <> h.Length
           && lenA <> i'.Length
           && lenA <> j.Length
        then failwith "Irregular arrays"
        else 
        let res = Array.zeroCreate lenA
        for i = 0 to res.Length - 1 do
            res.[i] <- (a.[i]
                       ,b.[i]
                       ,c.[i]
                       ,d.[i]
                       ,e.[i]
                       ,f.[i]
                       ,g.[i]
                       ,h.[i]
                       ,i'.[i]
                       ,j.[i])
        res

    let  Unzip (xs : ('a * 'b) array)
        : ('a array * 'b array) =
        Array.unzip xs

    let  Unzip3 (xs : ('a * 'b * 'c) array)
        : ('a array * 'b array * 'c array) =
        Array.unzip3 xs
        
    let  Unzip4 (xs : ('a * 'b * 'c * 'd) array)
        : ('a array * 'b array * 'c array * 'd array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
        res1, res2, res3, res4

    let  Unzip5 (xs : ('a * 'b * 'c * 'd * 'e) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
        res1, res2, res3, res4, res5

    let  Unzip6 (xs : ('a * 'b * 'c * 'd * 'e * 'f) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        let res6 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e,f) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
            res6.[i] <- f
        res1, res2, res3, res4, res5, res6

    let  Unzip7 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        let res6 = Array.zeroCreate len
        let res7 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e,f,g) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
            res6.[i] <- f
            res7.[i] <- g
        res1, res2, res3, res4, res5, res6, res7

    let  Unzip8 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        let res6 = Array.zeroCreate len
        let res7 = Array.zeroCreate len
        let res8 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e,f,g,h) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
            res6.[i] <- f
            res7.[i] <- g
            res8.[i] <- h
        res1, res2, res3, res4, res5, res6, res7, res8

    let  Unzip9 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array * 'i array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        let res6 = Array.zeroCreate len
        let res7 = Array.zeroCreate len
        let res8 = Array.zeroCreate len
        let res9 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e,f,g,h,i) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
            res6.[i] <- f
            res7.[i] <- g
            res8.[i] <- h
            res9.[i] <- i
        res1, res2, res3, res4, res5, res6, res7, res8, res9
            
    let  Unzip10 (xs : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) array)
        : ('a array * 'b array * 'c array * 'd array * 'e array * 'f array * 'g array * 'h array * 'i array * 'j array) =
        let len = xs.Length
        let res1 = Array.zeroCreate len
        let res2 = Array.zeroCreate len
        let res3 = Array.zeroCreate len
        let res4 = Array.zeroCreate len
        let res5 = Array.zeroCreate len
        let res6 = Array.zeroCreate len
        let res7 = Array.zeroCreate len
        let res8 = Array.zeroCreate len
        let res9 = Array.zeroCreate len
        let res10 = Array.zeroCreate len
        for i = 0 to xs.Length-1 do
            let (a,b,c,d,e,f,g,h,i,j) = xs.[i]
            res1.[i] <- a
            res2.[i] <- b
            res3.[i] <- c
            res4.[i] <- d
            res5.[i] <- e
            res6.[i] <- f
            res7.[i] <- g
            res8.[i] <- h
            res9.[i] <- i
            res10.[i] <- j
        res1, res2, res3, res4, res5, res6, res7, res8, res9, res10

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
        
