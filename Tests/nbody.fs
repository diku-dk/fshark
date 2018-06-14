namespace Nbody
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

module Internal =
open FSharkPrelude.FSharkPrelude
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

let wrap_body (posx : single) (posy : single) (posz : single) (mass : single) (velx : single)
              (vely : single) (velz : single) (accx : single) (accy : single) (accz : single)
              : body =
  {position={x=posx; y=posy; z=posz};
   mass=mass;
   velocity={x=velx; y=vely; z=velz};
   acceleration={x=accx; y=accy; z=accz}}

let unwrap_body (b: body): (single * single * single * single * single * single * single * single * single * single) =
  (b.position.x, b.position.y, b.position.z,
   b.mass,
   b.velocity.x, b.velocity.y, b.velocity.z,
   b.acceleration.x, b.acceleration.y, b.acceleration.z)

[<FSharkEntry>]
let main (n_steps : int) (epsilon : single) (time_step : single) 
         (xps : single array) (yps : single array) (zps : single array) (ms : single array) 
         (xvs : single array) (yvs : single array) (zvs : single array) 
         (xas : single array) (yas : single array) (zas : single array)
         : (single array * single array * single array * single array * single array * single array * single array * single array * single array * single array) =
         
  let bodies  = Map (fun (x,y,z,m,xv,yv,zv,xa,ya,za) -> 
                  wrap_body x y z m xv yv zv xa ya za
                ) <| Zip10 xps yps zps ms xvs yvs zvs xas yas zas
  let bodies' = advance_bodies_steps n_steps epsilon time_step bodies
  let bodies'' = Map unwrap_body (bodies')
  in Unzip10(bodies'')

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

let inverseRotatePoint ((x,y,z,x_rotation,y_rotation) : 
    (single * single * single * single * single)) : (single*single*single) =
  let vec = rotatePointByMatrix (inverseRotationMatrix x_rotation y_rotation) {x=x;y=y;z=z}
  in (vec.x,vec.z, vec.y)

let rotatePoints (ps: position array) (x_rotation: single) (y_rotation: single): position array =
  rotatePointsByMatrix (rotationMatrix x_rotation y_rotation) ps

