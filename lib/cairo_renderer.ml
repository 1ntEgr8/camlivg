open Vm
open Cairo

module Make (Vm : V) : Interpreter.I with type t = Vm.t = struct
  type t = Vm.t

  open Util
  open Util.Point

  type machine_state =
    { regs: int64 array
    ; mutable sel: int
    ; mutable galpha: float
    ; mutable gftm: sextuple
    ; mutable gra: int64
    ; eob: int64 }

  let init_machine () =
    { regs= Array.make 64 0x0000_00FF_0000_0000L
    ; sel= 56
    ; galpha= 0.
    ; gftm= (1., 0., 0., 0., 1., 0.)
    ; gra= 0L
    ; eob= 0xFFFF_FFFF_FFFF_FFFFL }

  let run_instructions vm ctx =
    let state = init_machine () in
    let gftm (x, y) =
      let fa, fb, fc, fd, fe, ff = state.gftm in
      ((fa *. x) +. (fb *. y) +. fc, (fd *. x) +. (fe *. y) +. ff)
    in
    try
      while true do
        let instruction, _ = Vm.next_instruction vm.pc in
        match instruction with
        | LineTo coords ->
            List.iter
              (fun a ->
                let x, y = gftm a in
                line_to ctx x y )
              coords
        | CubeTo coords ->
            List.iter
              (fun (x1, y1, x2, y2, x3, y3) ->
                let x1, y1 = gftm (x1, y1) in
                let x2, y2 = gftm (x2, y2) in
                let x3, y3 = gftm (x3, y3) in
                curve_to ctx x1 y1 x2 y2 x3 y3 )
              coords
        | ClosePathMoveTo (x, y) -> Path.close ctx ; move_to ctx x y
        | Parallelogram (b1, b2, c1, c2) ->
            let a = Path.get_current_point ctx in
            let b1, b2 = gftm (b1, b2) in
            let c1, c2 = gftm (c1, c2) in
            let d1, d2 = a -~ (b1, b2) +~ (c1, c2) in
            line_to ctx b1 b2 ;
            line_to ctx c1 c2 ;
            line_to ctx d1 d2 ;
            Path.close ctx
        | Ellipse (num_quarters, (b1, b2, c1, c2)) ->
            let ((a1, a2) as a) = Path.get_current_point ctx in
            let ((b1, b2) as b) = gftm (b1, b2) in
            let ((c1, c2) as c) = gftm (c1, c2) in
            let ((d1, d2) as d) = a -~ b +~ c in
            let x = (a +~ c) /~ 2. in
            let k = 0.551784777779014 in
            let r = b -~ x in
            let s = c -~ x in
            let am1, am2 = a -~ (r *~ k) in
            let ap1, ap2 = a +~ (r *~ k) in
            let bm1, bm2 = b -~ (s *~ k) in
            let bp1, bp2 = b +~ (s *~ k) in
            let cm1, cm2 = c +~ (r *~ k) in
            let cp1, cp2 = c -~ (r *~ k) in
            let dm1, dm2 = d +~ (s *~ k) in
            let dp1, dp2 = d -~ (s *~ k) in
            curve_to ctx ap1 ap2 bm1 bm2 b1 b2 ;
            if num_quarters >= 2 then curve_to ctx bp1 bp2 cm1 cm2 c1 c2 ;
            if num_quarters >= 3 then curve_to ctx cp1 cp2 dm1 dm2 d1 d2 ;
            if num_quarters >= 4 then curve_to ctx dp1 dp2 am1 am2 a1 a2
        | SelPlus incr -> state.sel <- (state.sel + incr) mod 64
        | SetRegHigh (low4, data) ->
            state.regs.(state.sel + low4) <-
              Int64.shift_left (Int64.of_int32 data) 32
        | SelSetRegs (low4, raw) ->
            let raw_stream = Stream.of_list raw in
            state.sel <- (state.sel - (low4 + 2)) mod 64 ;
            for i = 1 to low4 + 2 do
              let lo32 = Stream.next raw_stream in
              let hi32 = Stream.next raw_stream in
              let buf = Bytes.create 8 in
              List.iteri
                (fun i v -> Bytes.set buf i (char_of_int v))
                (lo32 @ hi32) ;
              state.regs.(state.sel + i) <- Bytes.get_int64_le buf 0
            done
        | FillFlat low4 ->
            let r, g, b, a =
              Color.postmul_rgba state.regs.(state.sel + low4)
            in
            save ctx;
            set_source_rgba ctx r g b a ;
            fill ctx ;
            Path.close ctx;
            restore ctx
        | _ -> ()
      done
    with Encdec.Decoder.OutOfOps -> ()

  (* TODO move this out of this module *)
  let setup_cairo _vm =
    let surface = Image.create Image.ARGB32 ~w:256 ~h:256 in
    let ctx = create surface in
    translate ctx 128. 128. ; scale ctx 2. 2. ; (surface, ctx)

  let run vm =
    let surface, ctx = setup_cairo vm in
    Vm.reset vm ;
    run_instructions vm ctx ;
    stroke ctx ;
    PNG.write surface "out.png"
end
