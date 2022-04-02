let built_in_palette =
  [| 0x00000000
   ; 0x80808080
   ; 0xC0C0C0C0
   ; 0xFF000000
   ; 0xFF000040
   ; 0xFF000080
   ; 0xFF0000C0
   ; 0xFF0000FF
   ; 0xFF004000
   ; 0xFF004040
   ; 0xFF004080
   ; 0xFF0040C0
   ; 0xFF0040FF
   ; 0xFF008000
   ; 0xFF008040
   ; 0xFF008080
   ; 0xFF0080C0
   ; 0xFF0080FF
   ; 0xFF00C000
   ; 0xFF00C040
   ; 0xFF00C080
   ; 0xFF00C0C0
   ; 0xFF00C0FF
   ; 0xFF00FF00
   ; 0xFF00FF40
   ; 0xFF00FF80
   ; 0xFF00FFC0
   ; 0xFF00FFFF
   ; 0xFF400000
   ; 0xFF400040
   ; 0xFF400080
   ; 0xFF4000C0
   ; 0xFF4000FF
   ; 0xFF404000
   ; 0xFF404040
   ; 0xFF404080
   ; 0xFF4040C0
   ; 0xFF4040FF
   ; 0xFF408000
   ; 0xFF408040
   ; 0xFF408080
   ; 0xFF4080C0
   ; 0xFF4080FF
   ; 0xFF40C000
   ; 0xFF40C040
   ; 0xFF40C080
   ; 0xFF40C0C0
   ; 0xFF40C0FF
   ; 0xFF40FF00
   ; 0xFF40FF40
   ; 0xFF40FF80
   ; 0xFF40FFC0
   ; 0xFF40FFFF
   ; 0xFF800000
   ; 0xFF800040
   ; 0xFF800080
   ; 0xFF8000C0
   ; 0xFF8000FF
   ; 0xFF804000
   ; 0xFF804040
   ; 0xFF804080
   ; 0xFF8040C0
   ; 0xFF8040FF
   ; 0xFF808000
   ; 0xFF808040
   ; 0xFF808080
   ; 0xFF8080C0
   ; 0xFF8080FF
   ; 0xFF80C000
   ; 0xFF80C040
   ; 0xFF80C080
   ; 0xFF80C0C0
   ; 0xFF80C0FF
   ; 0xFF80FF00
   ; 0xFF80FF40
   ; 0xFF80FF80
   ; 0xFF80FFC0
   ; 0xFF80FFFF
   ; 0xFFC00000
   ; 0xFFC00040
   ; 0xFFC00080
   ; 0xFFC000C0
   ; 0xFFC000FF
   ; 0xFFC04000
   ; 0xFFC04040
   ; 0xFFC04080
   ; 0xFFC040C0
   ; 0xFFC040FF
   ; 0xFFC08000
   ; 0xFFC08040
   ; 0xFFC08080
   ; 0xFFC080C0
   ; 0xFFC080FF
   ; 0xFFC0C000
   ; 0xFFC0C040
   ; 0xFFC0C080
   ; 0xFFC0C0C0
   ; 0xFFC0C0FF
   ; 0xFFC0FF00
   ; 0xFFC0FF40
   ; 0xFFC0FF80
   ; 0xFFC0FFC0
   ; 0xFFC0FFFF
   ; 0xFFFF0000
   ; 0xFFFF0040
   ; 0xFFFF0080
   ; 0xFFFF00C0
   ; 0xFFFF00FF
   ; 0xFFFF4000
   ; 0xFFFF4040
   ; 0xFFFF4080
   ; 0xFFFF40C0
   ; 0xFFFF40FF
   ; 0xFFFF8000
   ; 0xFFFF8040
   ; 0xFFFF8080
   ; 0xFFFF80C0
   ; 0xFFFF80FF
   ; 0xFFFFC000
   ; 0xFFFFC040
   ; 0xFFFFC080
   ; 0xFFFFC0C0
   ; 0xFFFFC0FF
   ; 0xFFFFFF00
   ; 0xFFFFFF40
   ; 0xFFFFFF80
   ; 0xFFFFFFC0
   ; 0xFFFFFFFF |]

let rgba_of_int n =
  let r = n land 0x000000FF |> float_of_int in
  let g = (n land 0x0000FF00) lsr 8 |> float_of_int in
  let b = (n land 0x00FF0000) lsr 16 |> float_of_int in
  let a = (n land 0xFF000000) lsr 24 |> float_of_int in
  (r, g, b, a)

let rgba_of_int64 n =
  let n = Int64.to_int (Int64.shift_right_logical n 32) in
  rgba_of_int n

let lin_blend_of_int64 n =
  let blend, colref0, colref1, _ = rgba_of_int64 n in
  (blend, int_of_float colref0, int_of_float colref1)

let rgba_is_sensible (r, g, b, a) =
  let comp_r = Float.compare r a in
  let comp_g = Float.compare g a in
  let comp_b = Float.compare b a in
  comp_r <= 0 && comp_g <= 0 && comp_b <= 0

let postmul_rgba (r, g, b, a) =
  if Float.equal a 0. then (0., 0., 0., 0.)
  else (r /. a, g /. a, b /. a, a /. 255.)

let blend (r0, g0, b0, a0) (r1, g1, b1, a1) weight =
  let weight0 = 255. -. weight in
  let weight1 = weight in
  let res chan0 chan1 =
    Float.floor (((weight0 *. chan0) +. (weight1 *. chan1) +. 128.) /. 255.)
  in
  (res r0 r1, res g0 g1, res b0 b1, res a0 a1)

let transparent_black = (0., 0., 0., 0.)

let opaque_black = (0., 0., 0., 1.)
