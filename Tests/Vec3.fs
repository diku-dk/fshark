module Vec3 =
    open Microsoft.FSharp.Math
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
