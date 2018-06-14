module sovs
type add2a = int * int -> int
let add2a (x,y,z) f = 
    printfn "%d" y
    printfn "%d" z
    printfn "%d" f
    x + 2

type add2b = int -> int
let add2b = ((+) 2)

let add2c x = ((+) 2) x

let const4 = 4

let a = 4
let b = a
let some_lista = [1;2;3]

let two_consts = (5,6)
let (a,b) = two_consts


let curried = fun foo -> foo + 5

let toTuple a b = (a,b)

let plus2 a = a + 2
let plus_p a = plus a
let plus_pipe a = (plus2 << plus2) a
let plus_comp a = plus2 <| plus2 a

let someMap xs = List.map (plus_p 2) xs
let someMap2 xs = Seq.map (plus_p 2) xs

let aLambda = fun (x,y) -> x+y

let funkyfun x =
    let a = 2
    let b = 5
    let fff = fun x y -> x + y
    // curried funs are turned into lambda x -> lambda y -> x + y
    in foo a x
    
let funkyfun2 x =
    let a = 2
    let b = 5
    let fff = fun (x,y) -> x + y
    // curried funs are turned into lambda x -> lambda y -> x + y
    in foo (a, x)

printfn "%s" "hej"
