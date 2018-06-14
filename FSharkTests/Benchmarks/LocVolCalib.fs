module LocVolCalib
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

let initGrid (s0: single) (alpha: single) (nu: single) (t: single) (numX: int) (numY: int) (numT: int)
  : (int * int * single array * single array * single array) =
  let logAlpha = log alpha
  let myTimeline = Map (fun i -> (t * (single i)) / ((single numT) - 1.0f)) (Iota numT)
  let (stdX, stdY) = (20.0f * alpha * s0 * sqrt(t),
                      10.0f * nu         * sqrt(t))
  let (dx, dy) = (stdX / single numX, stdY / single numY)
  let (myXindex, myYindex) = (int (s0 / dx), numY / 2)
  let myX = Map (fun i -> single i * dx - single myXindex * dx + s0) <| (Iota numX)
  let myY = Map (fun i -> single i * dy - single myYindex * dy + logAlpha) (Iota numY)
  in (myXindex, myYindex, myX, myY, myTimeline)

// make the innermost dimension of the result of size 4 instead of 3?
let initOperator (x: single array) : (single array array * single array array) = 
  let n = Length x
  let dxu     = x.[1] - x.[0]
  let dx_low  = [|[|0.0f; -1.0f / dxu; 1.0f / dxu|]|]
  let dxx_low = [|[|0.0f; 0.0f; 0.0f|]|]
  let dx_mids = Map (fun i -> 
                       let dxl = x.[i] - x.[i-1]
                       let dxu = x.[i+1] - x.[i]
                       in ([| -dxu/dxl/(dxl+dxu); (dxu/dxl - dxl/dxu)/(dxl+dxu);      dxl/dxu/(dxl+dxu) |],
                           [|  2.0f/dxl/(dxl+dxu); -2.0f*(1.0f/dxl + 1.0f/dxu)/(dxl+dxu); 2.0f/dxu/(dxl+dxu) |])) <|
                    [|1..(n-2)|]
                    
  let (dx_mid, dxx_mid) = Unzip dx_mids
  let dxl      = x.[n-1] - x.[n-2]
  let dx_high  = [|[|-1.0f / dxl; 1.0f / dxl; 0.0f |]|] : single array array
  let dxx_high = [|[|0.0f; 0.0f; 0.0f |]|] : single array array
  let dx     = Concat dx_low <| Concat dx_mid dx_high
  let dxx    = Concat dxx_low <| Concat dxx_mid dxx_high
  in  (dx, dxx)

let setPayoff (strike: single) (myX: single array) (_myY: single array) : single array array =
  let numY = Length _myY
  Replicate numY (Map (fun xi -> max (xi-strike) 0.0f) myX)

// Returns new myMuX, myVarX, myMuY, myVarY.
let updateParams (myX: single array) (myY : single array) (tnow: single) (_alpha: single) (beta: single) (nu: single)
  : (single array array * single array array * single array array * single array array) =
  let numX = Length myX
  let numY = Length myY
  let myMuY  = Replicate numX (Replicate numY 0.0f)
  let myVarY = Replicate numX (Replicate numY (nu*nu))
  let myMuX  = Replicate numY (Replicate numX 0.0f)
  let myVarX = Map (fun yj ->
                      Map (fun xi -> exp(2.0f*( beta * log(xi) + yj - 0.5f*nu*nu*tnow)))
                          myX)
                   myY
  in  ( myMuX, myVarX, myMuY, myVarY )

let tridagPar (a: single array) (b: single array) (c : single array) (y : single array) : single array =

  let n = Length a
(*    ----------------------------------------------------
 *    -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
 *    --   solved by scan with 2x2 matrix mult operator --
 *    ---------------------------------------------------- *)
  let b0   = b.[0]
  let mats = Map  (fun i ->
                     if 0 < i
                     then (b.[i], 0.0f-a.[i]*c.[i-1], 1.0f, 0.0f)
                     else (1.0f,  0.0f,             0.0f, 1.0f))
                  (Iota n)
  let scmt = Scan (fun (a0,a1,a2,a3) (b0,b1,b2,b3) ->
                     let value = 1.0f/(a0*b0)
                     in ( (b0*a0 + b1*a2)*value,
                          (b0*a1 + b1*a3)*value,
                          (b2*a0 + b3*a2)*value,
                          (b2*a1 + b3*a3)*value))
                  (1.0f,  0.0f, 0.0f, 1.0f) mats
  let b    = Map (fun (t0,t1,t2,t3) ->
                    (t0*b0 + t1) / (t2*b0 + t3))
                 scmt
(*    ------------------------------------------------------
      -- Recurrence 2: y[i] = y[i] - (a[i]/b[i-1])*y[i-1] --
      --   solved by scan with linear func comp operator  --
      ------------------------------------------------------ *)
  let y0   = y.[0]
  let lfuns= Map  (fun (i: int) ->
                     if 0 < i
                     then (y.[i], 0.0f-a.[i]/b.[i-1])
                     else (0.0f,  1.0f))
                  (Iota n)
  let cfuns= Scan (fun (a: (single*single)) (b: (single*single))  ->
                     let (a0,a1) = a
                     let (b0,b1) = b
                     in ( b0 + b1*a0, a1*b1 ))
                  (0.0f, 1.0f) lfuns
  let y    = Map (fun (tup: (single*single)) ->
                    let (a,b) = tup
                    in a + b*y0)
                 cfuns
(*    ------------------------------------------------------
      -- Recurrence 3: backward recurrence solved via     --
      --             scan with linear func comp operator  --
      ------------------------------------------------------ *)
  let yn   = y.[n-1]/b.[n-1]
  let lfuns= Map (fun (k: int) ->
                    let i = n-k-1
                    in  if   0 < k
                        then (y.[i]/b.[i], 0.0f-c.[i]/b.[i])
                        else (0.0f,       1.0f))
                 (Iota n)

  let cfuns= Scan (fun (a0,a1) (b0,b1) -> (b0 + b1*a0, a1*b1))
                  (0.0f, 1.0f) lfuns
  let y    = Map (fun (a,b) -> a + b*yn)
                 cfuns
  let y    = Map (fun i -> y.[n-i-1]) (Iota n)
  in y

let explicitMethod (myD:    single array array) (myDD: single array array)
                   (myMu:   single array array) (myVar: single array array)
                   (result: single array array)
                  : single array array =
  // 0 <= i < m AND 0 <= j < n
  let m = Length myDD
  Map3 (fun (mu_row : single array) (var_row : single array) (result_row : single array) ->
         Map5 (fun (dx : single array) (dxx : single array) (mu : single) (var : single) (j : int) ->
                let c1 = if 0 < j
                         then (mu*dx.[0] + 0.5f*var*dxx.[0]) * result_row.[j-1]
                         else 0.0f
                let c3 = if j < (m-1)
                         then (mu*dx.[2] + 0.5f*var*dxx.[2]) * result_row.[j+1]
                         else 0.0f
                let c2 = (mu*dx.[1] + 0.5f*var*dxx.[1]) * result_row.[j]
                in  c1 + c2 + c3)
             myD myDD mu_row var_row <| (Iota m))
      myMu myVar result

// for implicitY: should be called with transpose(u) instead of u
let implicitMethod (myD:  single array array)  (myDD:  single array array)
                   (myMu: single array array)  (myVar: single array array)
                   (u:    single array array)  (dtInv: single)
                    : single array array =
  Map3 (fun (mu_row : single array) (var_row : single array) (u_row : single array) ->
          let (a,b,c) = Unzip3 (
                          Map4 (fun mu var (d : single array) (dd : single array) ->
                                  (0.0f - 0.5f*(mu*d.[0] + 0.5f*var*dd.[0]), 
                                  dtInv - 0.5f*(mu*d.[1] + 0.5f*var*dd.[1]), 
                                  0.0f   - 0.5f*(mu*d.[2] + 0.5f*var*dd.[2])
                                  )
                               ) mu_row var_row myD myDD
                              )
          in tridagPar a b c u_row
       ) myMu myVar u

let rollback (tnow: single) (tnext: single) (myResult: single array array)
   (myMuX: single array array) (myDx: single array array) (myDxx: single array array) (myVarX : single array array)
   (myMuY: single array array) (myDy: single array array) (myDyy: single array array) (myVarY: single array array)
  : single array array =

  let dtInv = 1.0f/(tnext-tnow)

  // explicitX
  let u = explicitMethod myDx myDxx myMuX myVarX myResult
  let u = Map2 (Map2 (fun u_el res_el  -> dtInv*res_el + 0.5f*u_el))
               u myResult

  // explicitY
  let myResultTR = Transpose myResult
  let v = explicitMethod myDy myDyy myMuY myVarY myResultTR
  let u = Map2 (Map2 (+)) u (Transpose v)
  // implicitX
  let u = implicitMethod myDx myDxx myMuX myVarX u dtInv
  // implicitY
  let y = Map2 (fun u_row v_row ->
                 Map2 ( fun u_el v_el -> dtInv*u_el - 0.5f*v_el) u_row v_row)
              (Transpose u) v

  let myResultTR = implicitMethod myDy myDyy myMuY myVarY y dtInv
  in Transpose myResultTR

let value (numX: int) (numY: int) (numT: int) (s0: single) (strike: single) (t: single) (alpha: single) (nu: single) (beta: single): single =
  let (myXindex, myYindex, myX, myY, myTimeline) = initGrid s0 alpha nu t numX numY numT
  let (myDx, myDxx) = initOperator myX
  let (myDy, myDyy) = initOperator myY
  let myResult = setPayoff strike myX myY
  let myTimeline_neighbours = Reverse (Zip (Init myTimeline) (Tail myTimeline))

  let myResult' = 
    Foldl (fun oldResult (tnow,tnext) ->
      let (myMuX, myVarX, myMuY, myVarY) =
        updateParams myX myY tnow alpha beta nu
      in rollback tnow tnext oldResult
            myMuX myDx myDxx myVarX
            myMuY myDy myDyy myVarY
      ) myResult myTimeline_neighbours
  in myResult'.[myYindex].[myXindex]

[<FSharkEntry>]
let main (outer_loop_count: int) (numX: int) ( numY: int) ( numT: int) 
         (s0: single) (t: single) (alpha: single) (nu: single) (beta: single)
         : single [] =
  let strikes = Map (fun i -> 0.001f*single i) (Iota outer_loop_count)
  let res = Map (fun x -> value numX numY numT s0 x t alpha nu beta) strikes
  in res
  
[<FSharkInput>]
let valIn = [|16;32;256;256;0.03f;5.0f;0.2f;0.6f;0.5f|] : obj array


[<FSharkOutput>]
let valOut = 
   [| 0.0300001f;  0.0290001f;  0.0280001f;  0.0270001f;  0.026f;      0.0251064f;  0.0247889f;  0.0244714f; 
      0.0241539f;  0.0238364f;  0.0235189f;  0.0232014f;  0.0228839f;  0.0225664f;  0.0222744f;  0.02199f
   |] : single array
