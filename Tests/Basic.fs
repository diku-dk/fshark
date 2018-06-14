(*
type Testyboi = (int * int)

let myPlus a b = a + b

let myPlus2 a = myPlus 5 a

let mapPlus (xs : int array) : int array = FSharkMap ((+) 2) xs

type Vec3 = {x : float; y : float; z : float}
let getX (v : Vec3) : float = v.x
let setX (v : Vec3) : Vec3 = {v with x = 1.0}
let setXY (v : Vec3) : Vec3 = {v with x = 1.0; z = 4.0}

let prut = sqrt 4.0

let less = 1 < 2
let greater = 1 > 2
let leqq = 1 <= 2
let geqq = 1 >= 2
let eqq = 1 = 2
let nqeqq = 1 <> 2


let absedNum = abs <| -5
let acosed = acos 90.0
let asined = asin 90.0
let ataned = atan 90.0
*)
let int8d = int8 90.0
let atan2ed = atan2 90.0
let ceiled = ceil 90.0
let compareed = compare 90.0
let cosed = cos 90.0
let coshed = cosh 90.0
let exped = exp 90.0
let floated = float 90.0
let float32ed = float32 90.0
let floored = floor 90.0
let infinityed = 4.0 + (infinity : double)
let infinityfed = infinityf
let inted = int 90.0
let int8ed = int8 90.0
let int16ed = int16 90.0
let int32ed = int32 90.0
let int64ed = int64 90.0
let loged = log 90.0
let log10ed = log10 90.0
let maxed = max 90.0
let mined = min 90.0
let naned = nan
let nanfed = nanf

let noted = not true
let powned = pown 90.0
let rounded = round 90.0
let signed = sign 90.0
let sined = sin 90.0
let sinhed = sinh 90.0
let myTuple = (1,2)
let fsted = fst myTuple
let snded = snd myTuple
let sqrted = sqrt 90.0
let taned = tan 90.0
let tanhed = tanh 90.0
let truncateed = truncate 90.0
let uint8ed = uint8 90.0
let uint16ed = uint16 90.0
let uint32ed = uint32 90.0
let uint64ed = uint64 90.0


(*
type TEST = FOO // these are discriminated unions
          | BAR
*)
