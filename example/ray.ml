let delta = sqrt epsilon_float

type vec = {x:float; y:float; z:float}
let ( *| ) s r = {x = s *. r.x; y = s *. r.y; z = s *. r.z}
let ( +| ) a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z}
let ( -| ) a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z}
let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
let length r = sqrt(dot r r)
let unitise r = 1. /. length r *| r

type scene =
    Sphere of vec * float
  | Group of vec * float * scene * scene * scene * scene * scene

let ray_sphere {x=dx; y=dy; z=dz} {x=vx; y=vy; z=vz} r =
  let disc = vx *. vx +. vy *. vy +. vz *. vz -. r *. r in
  if disc < 0. then infinity else
    let b = vx *. dx +. vy *. dy +. vz *. dz in
    let b2 = b *. b in
    if b2 < disc then infinity else
      let disc = sqrt(b2 -. disc) in
      let t1 = b -. disc in
      if t1 > 0. then t1 else b +. disc

let ray_sphere' {x=ox; y=oy; z=oz} {x=dx; y=dy; z=dz} {x=cx; y=cy; z=cz} r =
  let vx = cx -. ox and vy = cy -. oy and vz = cz -. oz in
  let vv = vx *. vx +. vy *. vy +. vz *. vz in
  let b = vx *. dx +. vy *. dy +. vz *. dz in
  let disc = b *. b -. vv +. r *. r in
  disc >= 0. && b +. sqrt disc >= 0.

type hit = {l: float; nx: float; ny: float; nz: float}

let rec intersect ({x=dx; y=dy; z=dz} as dir) hit = function
    Sphere ({x=cx; y=cy; z=cz} as center, radius) ->
      let l' = ray_sphere dir center radius in
      if l' >= hit.l then hit else
	let x = l' *. dx -. cx in
	let y = l' *. dy -. cy in
	let z = l' *. dz -. cz in
	let il = 1. /. sqrt(x *. x +. y *. y +. z *. z) in
	{l = l'; nx = il *. x; ny = il *. y; nz = il *. z}
  | Group (center, radius, a, b, c, d, e) ->
      let l' = ray_sphere dir center radius in
      if l' >= hit.l then hit else
	let f h s = intersect dir h s in
	f (f (f (f (f hit a) b) c) d) e

let rec intersect' orig dir = function
    Sphere (center, radius) -> ray_sphere' orig dir center radius
  | Group (center, radius, a, b, c, d, e) ->
      let f s = intersect' orig dir s in
      ray_sphere' orig dir center radius && (f a || f b || f c || f d || f e)

let neg_light = unitise { x = 1.; y = 3.; z = -2. }

let rec ray_trace dir scene =
  let hit = intersect dir {l=infinity; nx=0.; ny=0.; nz=0.} scene in
  if hit.l = infinity then 0. else
    let n = {x = hit.nx; y = hit.ny; z = hit.nz} in
    let g = dot n neg_light in
    if g < 0. then 0. else
      if intersect' (hit.l *| dir +| delta *| n) neg_light scene then 0. else g

let fold5 f x a b c d e = f (f (f (f (f x a) b) c) d) e

let rec create level c r =
  let obj = Sphere (c, r) in
  if level = 1 then obj else
    let a = 3. *. r /. sqrt 12. in
    let rec bound (c, r) = function
	Sphere (c', r') -> c, max r (length (c -| c') +. r')
      | Group (_, _, v, w, x, y, z) -> fold5 bound (c, r) v w x y z in
    let aux x' z' = create (level - 1) (c +| {x=x'; y=a; z=z'}) (0.5 *. r) in
    let w = aux (-.a) (-.a) and x = aux a (-.a) in
    let y = aux (-.a) a and z = aux a a in
    let c, r = fold5 bound (c +| {x=0.; y=r; z=0.}, 0.) obj w x y z in
    Group (c, r, obj, w, x, y, z)

let string_init n f = 
  if n = 0 then "" else
    let s = String.create n in
    for i = 0 to n-1 do s.[i] <- f i done;
    s

let offset size degree i = i * (size/degree) + (min (size mod degree) i)

let raster scene ss l s d y =
  string_init s
    (fun x ->
       let g = ref 0. in
       for dx = 0 to ss - 1 do
	 for dy = 0 to ss - 1 do
	   let aux x d = float x -. float s /. 2. +. float d /. float ss in
	   let dir = unitise {x = aux x dx; y = aux y dy; z = float s } in
	   g := !g +. ray_trace dir scene
	 done
       done;
       let g = 0.5 +. 255. *. !g /. float (ss*ss) in
       char_of_int (int_of_float g))

let rasters l s d i =
  let scene = create l { x = 0.; y = -1.; z = 4. } 1. and ss = 4 in
  let off = offset s d i in
  (off, Array.init ((s-i-1)/d + 1) (fun j -> raster scene ss l s d (s-off-j)))

