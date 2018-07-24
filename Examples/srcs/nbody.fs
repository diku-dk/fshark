namespace Nbody
module Main =
    open FSharkPrelude.FSharkPrelude
    module Vec3 =
        type Vec3single = {x:single ; y:single ; z:single}
        let plus (a : Vec3single) (b : Vec3single) : Vec3single =
            {x=a.x+b.x; y=a.y+b.y; z=a.z+b.z}
                
        let minus (a : Vec3single) (b : Vec3single) : Vec3single =
            {x=a.x-b.x; y=a.y-b.y; z=a.z-b.z}
            
        let dot (a : Vec3single) (b : Vec3single) : single =
            a.x*b.x + a.y*b.y + a.z*b.z
            
        let scale (a : single) (b : Vec3single) : Vec3single =
            {x=a*b.x; y=a*b.y; z=a*b.z}
            
        let norm (a : Vec3single) : single =
            sqrt <| a.x*a.x + a.y*a.y + a.z*a.z
            
        let normalise (v : Vec3single) : Vec3single =
            let l = norm v
            in scale (1.0f / l) v
            
    type vec3 = Vec3.Vec3single
    type mass = single
    type position = vec3
    type acceleration = vec3
    type velocity = vec3
    
    type body = { position: position;
                  mass: mass;
                  velocity: velocity;
                  acceleration: acceleration
                }
    
    let accel (epsilon: single) (x:body) (y: body) : velocity =
      let r = Vec3.minus y.position x.position
      let rsqr = (Vec3.dot r r) + epsilon * epsilon
      let invr = 1.0f / sqrt rsqr
      let invr3 = invr * invr * invr
      let s = y.mass * invr3
      in Vec3.scale s r
    
    let calc_accels (epsilon: single) (bodies: body array): acceleration array =
      let move (body: body) =
        let accels = Map (accel epsilon body) bodies
        in Reduce_Comm (Vec3.plus) {x=0.0f; y=0.0f; z=0.0f} accels
      in Map move bodies
    
    let advance_body (time_step: single) (body: body) (acc: acceleration): body =
      let acceleration = Vec3.scale body.mass acc
      let position = Vec3.plus body.position <| Vec3.scale time_step body.velocity
      let velocity = Vec3.plus body.velocity <| Vec3.scale time_step acceleration
      in {position=position; mass=body.mass; velocity=velocity; acceleration=acceleration}
    
    let advance_bodies (epsilon: single) (time_step: single) (bodies: body array): body array =
      let accels = calc_accels epsilon bodies
      in Map2 (fun body acc -> advance_body time_step body acc) bodies accels
    
    let advance_bodies_steps (n_steps: int) (epsilon: single) (time_step: single)
                                 (bodies: body array): body array =
      let steps = Iota n_steps
      in Foldr (fun _ bodies' -> advance_bodies epsilon time_step bodies') bodies steps
    
    let wrap_body (posx : single ) ( posy : single ) ( posz : single) (mass : single) 
                  (velx : single ) ( vely : single ) ( velz : single) (accx : single ) ( accy : single ) ( accz : single)
                  : body =
      {position={x=posx; y=posy; z=posz};
       mass=mass;
       velocity={x=velx; y=vely; z=velz};
       acceleration={x=accx; y=accy; z=accz}}
    
    let unwrap_body (b: body): ((single * single * single) * single * (single * single * single) * (single * single * single)) =
      ((b.position.x, b.position.y, b.position.z), 
        b.mass, 
        (b.velocity.x, b.velocity.y, b.velocity.z), 
        (b.acceleration.x, b.acceleration.y, b.acceleration.z))
    
    [<FSharkEntry>]
    let main (n_steps : int) (epsilon : single) (time_step : single) 
             (xps : single array) (yps : single array) (zps : single array) (ms : single array) 
             (xvs : single array) (yvs : single array) (zvs : single array) 
             (xas : single array) (yas : single array) (zas : single array)
             : ((single array * single array * single array * single array) * (single array * single array * single array) * (single array * single array * single array)) =
             
      let bodies  = Map4 (fun (xp,yp,zp) m (xv, yv, zv) (xa,ya,za) -> wrap_body xp yp zp m xv yv zv xa ya za) (Zip3 xps yps zps) ms (Zip3 xvs yvs zvs) (Zip3 xas yas zas)
      let bodies' = advance_bodies_steps n_steps epsilon time_step bodies
      let bodies'' = Map unwrap_body (bodies')
      let (ps,ms,vs,acs) = Unzip4 bodies''
      let (xps, yps, zps) = Unzip3 ps
      let (xvs, yvs, zvs) = Unzip3 vs
      let (xas, yas, zas) = Unzip3 acs
      in ((xps, yps, zps, ms), (xvs, yvs, zvs), (xas, yas, zas))
    
    let rotatePointByMatrix (rotation: single array array) (p: position): position =
      let x = p.x
      let y = p.y
      let z = p.z
      {x= x*rotation.[0].[0] + y*rotation.[1].[0] + z*rotation.[2].[0];
       y= x*rotation.[0].[1] + y*rotation.[1].[1] + z*rotation.[2].[1];
       z= x*rotation.[0].[2] + y*rotation.[1].[2] + z*rotation.[2].[2]}
    
    let rotatePointsByMatrix (rotation: single array array) (ps: position array): position array =
      Map (rotatePointByMatrix rotation) ps
    
    let rotateXMatrix (angle: single): single array array =
      [|
        [|1.0f;       0.0f;          0.0f|];
        [|0.0f;  cos angle;   - sin angle|];
        [|0.0f;  sin angle;     cos angle|]
      |]
    
    let rotateYMatrix (angle: single): single array array =
      [|
        [|cos angle  ; 0.0f; sin angle|];
        [|0.0f       ; 1.0f; 0.0f     |];
        [|- sin angle; 0.0f; cos angle|]
      |]
    
    let matmult (x: single array array) (y: single array array) : single array array =
      let Sum = Reduce (+) 0.0f
      Map (fun xr ->
            Map (fun yc -> Sum (Map2 (fun x y -> x*y) xr yc)) (Transpose y)
                ) x
    
    let rotationMatrix (x_rotation: single) (y_rotation: single): single array array =
      matmult (rotateXMatrix x_rotation) (rotateYMatrix y_rotation)
    
    let inverseRotationMatrix (x_rotation: single) (y_rotation: single): single array array =
      matmult (rotateYMatrix y_rotation) (rotateXMatrix x_rotation)
    
    let inverseRotatePoint (x : single) (y : single) (z : single) (x_rotation : single) (y_rotation : single) : (single*single*single) =
      let vec = rotatePointByMatrix (inverseRotationMatrix x_rotation y_rotation) {x=x;y=y;z=z}
      in (vec.x,vec.z, vec.y)
    
    let rotatePoints (ps: position array) (x_rotation: single) (y_rotation: single): position array =
      rotatePointsByMatrix (rotationMatrix x_rotation y_rotation) ps
    (* 
    [<FSharkInput>]
    let inputData = 
            [|
                10;
                50.0f;
                0.1f;
                [|49.934765f; -19.72055f; -8.485103f; -40.529545f; 36.915504f; 58.12199f; -18.56515f; -16.319696f; -35.320797f; 35.467846f; 22.741138f; -19.066488f; -4.6187997f; -37.741123f; 36.013374f; 49.562435f; -9.462863f; -10.790539f; -40.558056f; 29.8883f; 32.482105f; -20.018032f; -12.006343f; -36.06932f; 36.564384f; 72.98635f; -6.637457f; -5.4672456f; -43.633926f; 54.48485f; 50.216465f; -24.18221f|]
                [|48.864784f; 21.0447f; 70.43111f; -23.411388f; -13.378705f; 47.11681f; 19.121122f; 68.65275f; -9.581309f; -31.458008f; 16.762173f; 32.53443f; 54.83043f; -23.498236f; -17.063395f; 83.03267f; 25.536043f; 66.63314f; -29.971233f; -20.165321f; 30.910307f; 19.918154f; 50.781525f; -13.634953f; -22.889074f; 36.489113f; 11.754714f; 71.23518f; -22.012821f; -44.066643f; 50.00622f; 1.1176014f|]
                [|49.504097f; 21.976465f; -66.07767f; -23.874193f; 22.885813f; 49.27831f; 19.856537f; -70.91549f; -15.5530615f; 57.63878f; 47.443493f; 19.284454f; -69.1615f; -29.302645f; 32.41684f; 83.920395f; 36.41799f; -67.998985f; -24.474981f; 45.04451f; 49.05143f; 27.472118f; -48.374542f; -28.24347f; 41.142605f; 44.127728f; 14.4537f; -59.5781f; -7.0467205f; 29.847332f; 49.413647f; 18.39111f|]
                [|3.456227f; 63.459526f; 74.34554f; 8.434827f; 16.776989f; 21.63172f; 69.831604f; 66.98223f; 23.477612f; 4.5274367f; 45.534313f; 37.293446f; 52.771004f; 83.92678f; 13.032608f; 18.12446f; 53.132633f; 94.62536f; 6.5728045f; 53.180336f; 91.3407f; 87.52959f; 75.542244f; 35.689384f; 77.35545f; 34.4798f; 20.805649f; 58.3743f; 20.659788f; 82.26547f; 86.262726f; 52.37195f|]
                [|10.561408f; 6.989762f; 14.306272f; -6.4595428f; -3.9690585f; 9.955754f; 6.6335444f; 13.727621f; -3.0388143f; -7.2828484f; 4.5114946f; 9.998227f; 11.664729f; -6.4404936f; -4.7614217f; 14.676114f; 7.573575f; 13.614646f; -8.00616f; -5.309554f; 7.583363f; 6.346644f; 12.0401945f; -3.9444003f; -5.9291406f; 7.5769863f; 5.286392f; 14.7713995f; -6.265288f; -10.098527f; 10.759963f; 0.40538555f|]
                [|-10.792668f; 6.5499606f; 1.7235309f; 11.182692f; -10.951718f; -12.281143f; 6.440665f; 3.263243f; 11.202367f; -8.211167f; -6.1207175f; 5.859364f; 0.9826122f; 10.344243f; -10.049282f; -8.760214f; 2.8065314f; 2.204749f; 10.834197f; -7.869626f; -7.9689794f; 6.378469f; 2.8466792f; 10.434348f; -9.471566f; -15.15566f; 2.9850323f; 1.1336936f; 12.4190855f; -12.486015f; -10.805201f; 8.771569f|]
                [|10.699586f; 7.2992377f; -13.421982f; -6.5872374f; 6.7895308f; 10.4124775f; 6.8886757f; -14.180073f; -4.932819f; 13.343963f; 12.769291f; 5.926348f; -14.713549f; -8.03139f; 9.045693f; 14.83302f; 10.800983f; -13.893719f; -6.5379558f; 11.860275f; 12.034005f; 8.753611f; -11.469504f; -8.17044f; 10.6575f; 9.163149f; 6.5001945f; -12.354176f; -2.0056372f; 6.8399606f; 10.632457f; 6.670974f|]
                [|-0.2020692f; 3.0261776f; -0.1371406f; 0.42419815f; -0.9118709f; -1.5379857f; 2.988307f; 1.625271f; 1.208595f; -0.12642522f; -1.232773f; 1.6313204f; -0.81280035f; 3.5243607f; -0.61353976f; -0.4695076f; 1.3853245f; 0.56374645f; 0.30523086f; -1.4631323f; -3.6304643f; 4.4817963f; 0.74818593f; 1.5595063f; -3.1575167f; -3.1058714f; 6.1454386e-2f; -0.6793159f; 1.3458371f; -4.644851f; -5.0198126f; 2.865766f|]
                [|-0.18428235f; -0.7999116f; -3.710925f; 0.5393226f; 0.8006712f; -0.9758822f; -0.41186926f; -2.7936146f; 1.1207385f; 0.3105037f; 2.6571987e-2f; -1.8151838f; -0.112044565f; 5.315734f; 0.70832497f; -1.0319139f; -1.4187478f; -3.55292f; 0.47446692f; 3.3048942f; -2.5821278f; -0.7540304f; -0.64892244f; 1.77316f; 4.8282127f; -0.73178864f; 0.3914103f; -3.241285f; 1.3664191f; 6.0297318f; -4.7489667f; 2.5242555f|]
                [|-0.13120669f; -1.3126944f; 3.796874f; 0.31401542f; 0.2444f; -0.7344652f; -0.9475892f; 4.267117f; 0.7841558f; -0.2921983f; -2.692973f; -0.52129644f; 3.5451076f; 3.9011729f; -0.11806177f; -1.0167954f; -3.0760522f; 5.51218f; 0.23335022f; -2.3880641f; -4.8232713f; -3.3089616f; 1.946604f; 1.7561425f; -2.4603796f; -0.6773815f; 0.19268043f; 1.9731628f; 0.22117926f; -0.19813985f; -3.2173421f; -0.771268f|]
            |] : obj array
        
    [<FSharkOutput>]
    let outputData = 
      (
        ([|60.402256f, -11.430504f, 5.672099f, -46.79211f, 32.574455f, 67.3728f, -10.644274f, -1.9366095f, -37.81175f, 28.138607f, 26.732052f, -8.415286f, 6.675616f, -42.53744f, 31.007177f, 64.01576f, -1.29756f, 2.984476f, -48.420025f, 24.05571f, 38.412075f, -11.713844f, 0.29591668f, -39.304882f, 29.410185f, 79.16618f, -1.3194776f, 8.919601f, -49.27589f, 42.495594f, 58.644115f, -22.410553f|],
         [|37.99447f, 27.125307f, 70.57071f, -12.007825f, -23.946268f, 34.43472f, 25.257013f, 70.68937f, 2.0563722f, -39.52876f, 10.697684f, 37.529408f, 55.826862f, -10.963094f, -26.778881f, 73.82858f, 27.675724f, 67.32786f, -18.939562f, -26.526985f, 21.907555f, 25.808075f, 53.392735f, -2.4880366f, -30.127684f, 21.078148f, 14.902553f, 70.99377f, -9.043354f, -53.756332f, 37.20186f, 10.888989f|],
         [|60.14521f, 28.753597f, -77.842896f, -30.323624f, 29.79417f, 59.36437f, 26.39887f, -83.21151f, -20.14278f, 70.849945f, 58.990242f, 25.032337f, -82.31381f, -35.619644f, 41.412075f, 98.300285f, 45.853508f, -79.47979f, -30.91017f, 55.817623f, 58.91943f, 34.806118f, -59.070557f, -35.646812f, 50.689945f, 52.9975f, 21.06105f, -71.09529f, -8.953376f, 36.63188f, 58.620056f, 24.760492f|],
         [|3.456227f, 63.459526f, 74.34554f, 8.434827f, 16.776989f, 21.63172f, 69.831604f, 66.98223f, 23.477612f, 4.5274367f, 45.534313f, 37.293446f, 52.771004f, 83.92678f, 13.032608f, 18.12446f, 53.132633f, 94.62536f, 6.5728045f, 53.180336f, 91.3407f, 87.52959f, 75.542244f, 35.689384f, 77.35545f, 34.4798f, 20.805649f, 58.3743f, 20.659788f, 82.26547f, 86.262726f, 52.37195f|]),
        ([|10.347675f, 9.780841f, 13.856287f, -6.012946f, -4.7382054f, 8.367946f, 9.401735f, 15.068537f, -1.817153f, -7.370752f, 3.4046803f, 11.330664f, 10.847925f, -2.7064044f, -5.2589273f, 14.1636095f, 8.84061f, 13.839526f, -7.6758323f, -6.2717843f, 3.8768096f, 10.581014f, 12.523133f, -2.3586354f, -8.353604f, 4.4726243f, 5.366333f, 13.811482f, -4.8574266f, -13.994319f, 5.4610834f, 3.5517094f|],
         [|-10.957504f, 5.350463f, -1.666952f, 11.642888f, -10.062807f, -13.115515f, 5.590103f, 0.5764816f, 12.071388f, -7.898251f, -5.9327974f, 3.876094f, 1.0917499f, 14.917309f, -9.285036f, -9.714817f, 1.2858781f, -1.0205598f, 11.250289f, -4.4937367f, -10.066701f, 5.0761976f, 2.3962336f, 11.898649f, -4.429852f, -15.612979f, 3.3273313f, -1.798587f, 13.547301f, -6.1497493f, -15.029127f, 10.775686f|],
         [|10.569622f, 6.240201f, -9.809946f, -6.2869954f, 7.0655513f, 9.689009f, 6.2358103f, -10.042387f, -4.190213f, 13.046776f, 10.0338125f, 5.60827f, -11.286869f, -4.294997f, 8.936092f, 13.833233f, 7.796603f, -8.638148f, -6.3134418f, 9.426997f, 7.232523f, 5.7207565f, -9.897869f, -6.5040054f, 8.189255f, 8.523445f, 6.76692f, -10.556653f, -1.7893666f, 6.7633076f, 7.486924f, 6.068233f|]),
        ([|-0.22668193f, 2.5433345f, -0.7218549f, 0.46778414f, -0.6296934f, -1.6421701f, 2.5346236f, 1.0556761f, 1.2293873f, -5.1867843e-2f, -0.98696345f, 1.0490761f, -0.7843886f, 3.9198356f, -0.38566536f, -0.55458456f, 1.1496811f, -8.429852e-2f, 0.35382912f, -0.48020265f, -3.7875612f, 3.913912f, 0.258676f, 1.6116967f, -1.6932052f, -3.1038995f, 0.10942383f, -1.1980586f, 1.4573994f, -3.1410325f, -5.588184f, 3.405976f|],
         [|-0.14637746f, -1.5698189f, -3.07428f, 0.38886592f, 0.97357625f, -0.6976927f, -1.2643027f, -2.6000845f, 0.63759214f, 0.31481925f, 0.33534586f, -2.1232f, 0.27396658f, 3.8610702f, 0.8186842f, -0.8758056f, -1.6105593f, -2.9109285f, 0.36280265f, 3.4272532f, -1.6009333f, -1.8188158f, -0.2904919f, 1.186789f, 5.2191257f, -0.19130452f, 0.29500887f, -2.6337848f, 0.90100104f, 6.6301613f, -3.6724644f, 1.4547812f|],
         [|-0.13127826f, -0.8167556f, 3.454369f, 0.2849534f, 0.30322555f, -0.72507f, -0.3754633f, 4.0255356f, 0.687956f, -0.30228883f, -2.785121f, -0.13574754f, 3.3349295f, 3.5435603f, -0.10571819f, -0.98143333f, -2.9288754f, 4.988788f, 0.21467017f, -2.4739354f, -4.7642846f, -2.7121327f, 1.2244126f, 1.5703841f, -2.4670205f, -0.6173794f, 0.33297652f, 1.6664244f, 0.20331861f, 3.0234138e-2f, -3.101759f, -0.44147408f|]
        )
      )
      
    *)
