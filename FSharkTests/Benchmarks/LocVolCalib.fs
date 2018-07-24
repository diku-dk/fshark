module LocVolCalib
open FSharkPrelude.FSharkPrelude
open FShark.TestTypes.TestTypes

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
(*----------------------------------------------------
  -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
  --   solved by scan with 2x2 matrix mult operator --
  ---------------------------------------------------- *)
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
(*------------------------------------------------------
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
(*------------------------------------------------------
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



  
(*
// small.in
[<FSharkInput>]
let valIn = [|16;32;256;256;0.03f;5.0f;0.2f;0.6f;0.5f|] : obj array

[<FSharkOutput>]
let valOut = 
   [| 0.0300001f;  0.0290001f;  0.0280001f;  0.0270001f;  0.026f;      0.0251064f;  0.0247889f;  0.0244714f; 
      0.0241539f;  0.0238364f;  0.0235189f;  0.0232014f;  0.0228839f;  0.0225664f;  0.0222744f;  0.02199f
   |] : single array
*)
   
(*
// medium.in
[<FSharkInput>]
let valIn = [|128;256;32;64;0.03f;5.0f;0.2f;0.6f;0.5f|] : obj array

[<FSharkOutput>]
let valOut = 
    [| 0.03f;       0.0292079f;  0.0288058f;  0.028408f;   0.0280152f;  0.0276279f;  0.0272462f;  0.0268701f;
      0.0264998f;  0.0261355f;  0.025777f;   0.0254243f;  0.0250773f;  0.024736f;   0.0244004f;  0.0240704f;
      0.0237459f;  0.0234269f;  0.0231134f;  0.0228053f;  0.0225025f;  0.022205f;   0.0219126f;  0.0216253f;
      0.0213431f;  0.0210657f;  0.0207928f;  0.0205238f;  0.0202581f;  0.0199978f;  0.0197739f;  0.0194946f; 
      0.0192514f;  0.0190136f;  0.0187787f;  0.018547f;   0.0183189f;  0.0180947f;  0.0178743f;  0.0176577f;
      0.0174448f;  0.0172356f;  0.0170299f;  0.0168278f;  0.016629f;   0.0164336f;  0.0162415f;  0.0160526f;
      0.0158668f;  0.0156841f;  0.0155044f;  0.0153276f;  0.0151536f;  0.0149826f;  0.0148144f;  0.0146488f;
      0.0144858f;  0.0143253f;  0.0141674f;  0.0140119f;  0.0138588f;  0.013708f;   0.0135595f;  0.0134132f;
      0.0132691f;  0.0131271f;  0.0129872f;  0.0128494f;  0.0127136f;  0.0125796f;  0.0124477f;  0.0123175f;
      0.0121892f;  0.0120627f;  0.0119379f;  0.0118149f;  0.0116936f;  0.0115739f;  0.0114558f;  0.0113392f;
      0.0112242f;  0.0111107f;  0.0109986f;  0.0108879f;  0.0107787f;  0.0106708f;  0.0105642f;  0.010459f;
      0.010355f;   0.0102523f;  0.0101509f;  0.0100506f;  0.00995151f; 0.00985357f; 0.00975677f; 0.00966107f;
      0.00956649f; 0.00947299f; 0.00938053f; 0.00928908f; 0.00919862f; 0.00910914f; 0.00902062f; 0.00893303f; 
      0.00884636f; 0.00876059f; 0.00867569f; 0.00859167f; 0.00850848f; 0.00842613f; 0.00834459f; 0.00826384f; 
      0.00818388f; 0.00810468f; 0.00802623f; 0.00794852f; 0.00787153f; 0.00779524f; 0.00771969f; 0.00764481f;
      0.0075706f;  0.00749704f; 0.00742412f; 0.00735183f; 0.00728016f; 0.00720909f; 0.00713861f; 0.00706872f
    |] : single array
*)

// large.in
[<FSharkInput>]
let valIn = [|256;256;256;64;0.03f;5.0f;0.2f;0.6f;0.5f|] : obj array

[<FSharkOutput>]
let valOut = 
    [|0.029998f; 0.029206f; 0.028804f; 0.028407f; 0.028014f; 0.027628f; 0.027246f;
    0.026871f; 0.026501f; 0.026137f; 0.025780f; 0.025427f; 0.025081f; 0.024741f;
    0.024406f; 0.024076f; 0.023753f; 0.023434f; 0.023122f; 0.022814f; 0.022512f;
    0.022215f; 0.021924f; 0.021637f; 0.021356f; 0.021079f; 0.020807f; 0.020539f;
    0.020274f; 0.020015f; 0.019792f; 0.019513f; 0.019271f; 0.019034f; 0.018800f;
    0.018569f; 0.018342f; 0.018118f; 0.017899f; 0.017683f; 0.017471f; 0.017262f;
    0.017057f; 0.016856f; 0.016658f; 0.016463f; 0.016272f; 0.016084f; 0.015899f;
    0.015717f; 0.015538f; 0.015362f; 0.015189f; 0.015019f; 0.014851f; 0.014686f;
    0.014524f; 0.014364f; 0.014207f; 0.014052f; 0.013900f; 0.013750f; 0.013602f;
    0.013457f; 0.013313f; 0.013172f; 0.013033f; 0.012895f; 0.012760f; 0.012627f;
    0.012496f; 0.012366f; 0.012238f; 0.012112f; 0.011988f; 0.011866f; 0.011745f;
    0.011626f; 0.011508f; 0.011392f; 0.011278f; 0.011165f; 0.011053f; 0.010943f;
    0.010834f; 0.010727f; 0.010621f; 0.010516f; 0.010412f; 0.010310f; 0.010209f;
    0.010109f; 0.010010f; 0.009913f; 0.009816f; 0.009721f; 0.009626f; 0.009533f;
    0.009441f; 0.009350f; 0.009260f; 0.009170f; 0.009082f; 0.008995f; 0.008908f;
    0.008822f; 0.008738f; 0.008654f; 0.008571f; 0.008489f; 0.008407f; 0.008326f;
    0.008247f; 0.008167f; 0.008089f; 0.008011f; 0.007934f; 0.007858f; 0.007783f;
    0.007708f; 0.007634f; 0.007560f; 0.007487f; 0.007415f; 0.007343f; 0.007272f;
    0.007201f; 0.007131f; 0.007062f; 0.006993f; 0.006925f; 0.006857f; 0.006790f;
    0.006723f; 0.006656f; 0.006591f; 0.006525f; 0.006461f; 0.006396f; 0.006332f;
    0.006269f; 0.006206f; 0.006143f; 0.006081f; 0.006019f; 0.005958f; 0.005897f;
    0.005837f; 0.005776f; 0.005717f; 0.005657f; 0.005598f; 0.005539f; 0.005481f;
    0.005423f; 0.005365f; 0.005308f; 0.005251f; 0.005194f; 0.005138f; 0.005082f;
    0.005026f; 0.004971f; 0.004915f; 0.004860f; 0.004806f; 0.004752f; 0.004697f;
    0.004644f; 0.004590f; 0.004537f; 0.004484f; 0.004431f; 0.004378f; 0.004326f;
    0.004274f; 0.004222f; 0.004170f; 0.004119f; 0.004068f; 0.004017f; 0.003966f;
    0.003915f; 0.003865f; 0.003815f; 0.003765f; 0.003715f; 0.003665f; 0.003616f;
    0.003567f; 0.003518f; 0.003469f; 0.003420f; 0.003371f; 0.003323f; 0.003274f;
    0.003226f; 0.003178f; 0.003130f; 0.003083f; 0.003035f; 0.002988f; 0.002940f;
    0.002893f; 0.002846f; 0.002799f; 0.002753f; 0.002706f; 0.002659f; 0.002613f;
    0.002567f; 0.002520f; 0.002474f; 0.002428f; 0.002382f; 0.002337f; 0.002291f;
    0.002245f; 0.002200f; 0.002154f; 0.002109f; 0.002064f; 0.002019f; 0.001974f;
    0.001929f; 0.001884f; 0.001839f; 0.001794f; 0.001749f; 0.001705f; 0.001660f;
    0.001616f; 0.001571f; 0.001527f; 0.001482f; 0.001438f; 0.001394f; 0.001350f;
    0.001306f; 0.001262f; 0.001218f; 0.001174f; 0.001130f; 0.001086f; 0.001042f;
    0.000998f; 0.000954f; 0.000911f; 0.000867f; 0.000823f; 0.000780f; 0.000736f;
    0.000692f; 0.000649f; 0.000605f; 0.000562f|] : single array
