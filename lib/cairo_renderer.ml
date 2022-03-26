open Vm
open Cairo

module Make (Vm : V) : Interpreter.I with type t = Vm.t = struct
  type t = Vm.t

  open Util.Point

  let run_instructions vm ctx =
    try
      while true do
        let instruction, _ = Vm.next_instruction vm.pc in
        match instruction with
        | LineTo coords ->
            List.iter (fun (x, y) ->
              line_to ctx x y
            ) coords
        | CubeTo coords ->
            List.iter (fun (x1, y1, x2, y2, x3, y3) ->
              curve_to ctx x1 y1 x2 y2 x3 y3
            ) coords
        | ClosePathMoveTo (x, y) ->
          Path.close ctx;
          move_to ctx x y 
        | Ellipse (num_quarters, (b1, b2, c1, c2)) ->
            let (a1, a2) as a = Path.get_current_point ctx in
            let b = (b1, b2) in
            let c = (c1, c2) in
            let (d1, d2) as d = a -~ b +~ c in
            let x = (a +~ c) /~ 2. in
            let k = 0.551784777779014 in
            let r = b -~ x in
            let s = c -~ x in
            let (am1, am2) = a -~ (r *~ k) in
            let (ap1, ap2) = a +~ (r *~ k) in
            let (bm1, bm2) = b -~ (s *~ k) in
            let (bp1, bp2) = b +~ (s *~ k) in
            let (cm1, cm2) = c +~ (r *~ k) in
            let (cp1, cp2) = c -~ (r *~ k) in
            let (dm1, dm2) = d +~ (s *~ k) in
            let (dp1, dp2) = d -~ (s *~ k) in
            curve_to ctx ap1 ap2 bm1 bm2 b1 b2;
            if num_quarters >= 2 then
              curve_to ctx bp1 bp2 cm1 cm2 c1 c2;
            if num_quarters >= 3 then
              curve_to ctx cp1 cp2 dm1 dm2 d1 d2;
            if num_quarters >= 4 then
              curve_to ctx dp1 dp2 am1 am2 a1 a2
        | Parallelogram (b1, b2, c1, c2) ->
            let a = Path.get_current_point ctx in 
            let (d1, d2) = a -~ (b1, b2) +~ (c1, c2) in
            line_to ctx b1 b2;
            line_to ctx c1 c2;
            line_to ctx d1 d2;
            Path.close ctx
        | FillFlat _low4 ->
            fill ctx;
            Path.close ctx
        | _ -> ()
      done
    with Encdec.Decoder.OutOfOps -> ()

  (* TODO move this out of this module *)
  let setup_cairo _vm =
    let surface = Image.create Image.ARGB32 ~w:256 ~h:256 in
    let ctx = create surface in
    translate ctx 128. 128.;
    scale ctx 2. 2.;
    (surface, ctx)

  let run vm =
    let (surface, ctx) = setup_cairo vm in
    Vm.reset vm;
    run_instructions vm ctx;
    stroke ctx;
    PNG.write surface "out.png"
end
