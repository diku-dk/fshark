namespace LocVolCalib
open FSharkPrelude.FSharkPrelude
module LocVolCalib =
    let initGrid (s0: float32) (alpha: float32) (nu: float32) (t: float32) (numX: int) (numY: int) (numT: int)
      : (int * int * float32 array * float32 array * float32 array) =
      let logAlpha = log alpha
      let myTimeline = Map (fun i -> (t * (float32 i)) / ((float32 numT) - 1.0f)) (Iota numT)
      let (stdX, stdY) = (20.0f * alpha * s0 * sqrt(t),
                          10.0f * nu         * sqrt(t))
      let (dx, dy) = (stdX / float32 numX, stdY / float32 numY)
      let (myXindex, myYindex) = (int (s0 / dx), numY / 2)
      let myX = Map (fun i -> float32 i * dx - float32 myXindex * dx + s0) <| (Iota numX)
      let myY = Map (fun i -> float32 i * dy - float32 myYindex * dy + logAlpha) (Iota numY)
      in (myXindex, myYindex, myX, myY, myTimeline)
    
    // make the innermost dimension of the result of size 4 instead of 3?
    let initOperator (x: float32 array) : (float32 array array * float32 array array) = 
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
      let dx_high  = [|[|-1.0f / dxl; 1.0f / dxl; 0.0f |]|] : float32 array array
      let dxx_high = [|[|0.0f; 0.0f; 0.0f |]|] : float32 array array
      let dx     = Concat dx_low <| Concat dx_mid dx_high
      let dxx    = Concat dxx_low <| Concat dxx_mid dxx_high
      in  (dx, dxx)

    let setPayoff (strike: float32) (myX: float32 array) (_myY: float32 array) : float32 array array =
      let numY = Length _myY
      Replicate numY (Map (fun xi -> max (xi-strike) 0.0f) myX)

    // Returns new myMuX, myVarX, myMuY, myVarY.
    let updateParams (myX: float32 array) (myY : float32 array) (tnow: float32) (_alpha: float32) (beta: float32) (nu: float32)
      : (float32 array array * float32 array array * float32 array array * float32 array array) =
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

    let tridagPar (a: float32 array) (b: float32 array) (c : float32 array) (y : float32 array) : float32 array =
    
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
      let cfuns= Scan (fun (a: (float32*float32)) (b: (float32*float32))  ->
                         let (a0,a1) = a
                         let (b0,b1) = b
                         in ( b0 + b1*a0, a1*b1 ))
                      (0.0f, 1.0f) lfuns
      let y    = Map (fun (tup: (float32*float32)) ->
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

      let cfuns= Scan (fun (a: (float32*float32)) (b: (float32*float32)) ->
                         let (a0,a1) = a
                         let (b0,b1) = b
                         in (b0 + b1*a0, a1*b1))
                      (0.0f, 1.0f) lfuns
      let y    = Map (fun (tup: (float32 * float32)) ->
                        let (a,b) = tup
                        in a + b*yn)
                     cfuns
      let y    = Map (fun i -> y.[n-i-1]) (Iota n)
      in y

    let explicitMethod (myD:    float32 array array) (myDD: float32 array array)
                       (myMu:   float32 array array) (myVar: float32 array array)
                       (result: float32 array array)
                      : float32 array array =
      // 0 <= i < m AND 0 <= j < n
      let m = Length myDD
      Map3 (fun (mu_row : float32 array) (var_row : float32 array) (result_row : float32 array) ->
             Map5 (fun (dx : float32 array) (dxx : float32 array) (mu : float32) (var : float32) (j : int) ->
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
    let implicitMethod (myD:  float32 array array)  (myDD:  float32 array array)
                       (myMu: float32 array array)  (myVar: float32 array array)
                       (u:    float32 array array)  (dtInv: float32)
                        : float32 array array =
      Map3 (fun (mu_row : float32 array) (var_row : float32 array) (u_row : float32 array) ->
              let (a,b,c) = Unzip3 (
                              Map4 (fun (mu : float32) (var : float32) (d : float32 array) (dd : float32 array) ->
                                      (0.0f - 0.5f*(mu*d.[0] + 0.5f*var*dd.[0]), dtInv - 0.5f*(mu*d.[1] + 0.5f*var*dd.[1]), 0.0f   - 0.5f*(mu*d.[2] + 0.5f*var*dd.[2])
                                      )
                                   ) mu_row var_row myD myDD
                                  )
              in tridagPar a b c u_row
           ) myMu myVar u
    
    let rollback (tnow: float32) (tnext: float32) (myResult: float32 array array)
       (myMuX: float32 array array) (myDx: float32 array array) (myDxx: float32 array array) (myVarX : float32 array array)
       (myMuY: float32 array array) (myDy: float32 array array) (myDyy: float32 array array) (myVarY: float32 array array)
      : float32 array array =
    
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
    
    let value (numX: int) (numY: int) (numT: int) (s0: float32) (strike: float32) (t: float32) (alpha: float32) (nu: float32) (beta: float32): float32 =
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
             (s0: float32) (t: float32) (alpha: float32) (nu: float32) (beta: float32)
             : float32 [] =
      let strikes = Map (fun i -> 0.001f*float32 i) (Iota outer_loop_count)
      let res = Map (fun x -> value numX numY numT s0 x t alpha nu beta) strikes
      in res
