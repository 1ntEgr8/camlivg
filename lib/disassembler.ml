open Util
open Ty
open Vm

module Make (Vm : V) : Interpreter.I with type t = Vm.t = struct
  type t = Vm.t

  open Printf

  let print_indent n =
    let indentation = List.map (fun _ -> " ") (1 -- n) |> String.concat "" in
    printf "%s" indentation

  let indent_level_1 = 3

  let indent_level_2 = indent_level_1 + 6

  let raw_byte_data_width = 8 + 3

  let is_geometry_instr i =
    match i with LineTo _ | QuadTo _ | CubeTo _ -> true | _ -> false

  let string_of_spread spread =
    if spread = 0x00 then "none"
    else if spread == 0x01 then "pad"
    else if spread == 0x02 then "reflect"
    else if spread == 0x03 then "repeat"
    else "unknown"

  let print_lo32 xs =
    let xs_stream = Stream.of_list xs in
    let b0 = Stream.next xs_stream in
    let b1 = Stream.next xs_stream in
    let b2 = Stream.next xs_stream in
    let b3 = Stream.next xs_stream in
    printf "0x%02x%02x_%02x%02x" b3 b2 b1 b0

  let print_hi32 xs =
    let xs_stream = Stream.of_list xs in
    let b0 = Stream.next xs_stream in
    let b1 = Stream.next xs_stream in
    let b2 = Stream.next xs_stream in
    let b3 = Stream.next xs_stream in
    printf "rgba(%02x:%02x:%02x:%02x)" b0 b1 b2 b3

  let print_hi32_of_int32 n =
    let b0 = Int32.to_int (Int32.shift_right_logical n 24) in
    let b1 =
      Int32.to_int
        (Int32.logand (Int32.shift_right_logical n 16) (Int32.of_int 0xFF))
    in
    let b2 =
      Int32.to_int
        (Int32.logand (Int32.shift_right_logical n 8) (Int32.of_int 0xFF))
    in
    let b3 = Int32.to_int (Int32.logand n (Int32.of_int 0xFF)) in
    printf "rgba(%02x:%02x:%02x:%02x)" b3 b2 b1 b0

  let print_raw_bytes buf =
    let out =
      List.map (fun b -> sprintf "%02x" b) buf |> String.concat " "
    in
    printf "%-11s" out

  let print_next_decoded raw_stream =
    print_raw_bytes (Stream.next raw_stream)

  let print_float raw_stream n =
    print_next_decoded raw_stream ;
    print_indent indent_level_2 ;
    printf "%+g\n" n

  let print_tuple raw_stream (c1, c2) =
    print_float raw_stream c1 ; print_float raw_stream c2

  let print_triple raw_stream (c1, c2, c3) =
    print_float raw_stream c1 ;
    print_float raw_stream c2 ;
    print_float raw_stream c3

  let print_quadruple raw_stream (c1, c2, c3, c4) =
    print_float raw_stream c1 ;
    print_float raw_stream c2 ;
    print_float raw_stream c3 ;
    print_float raw_stream c4

  let print_sextuple raw_stream (c1, c2, c3, c4, c5, c6) =
    print_float raw_stream c1 ;
    print_float raw_stream c2 ;
    print_float raw_stream c3 ;
    print_float raw_stream c4 ;
    print_float raw_stream c5 ;
    print_float raw_stream c6

  let print_reps p raw_stream xs =
    let num_reps = List.length xs in
    List.iteri
      (fun idx x ->
        p raw_stream x ;
        if idx < num_reps - 1 then (
          print_indent (indent_level_2 + raw_byte_data_width) ;
          printf "(rep)\n" ) )
      xs

  let print_magic vm =
    let get_byte idx = int_of_char (Bytes.get vm.magic idx) in
    if Bytes.length vm.magic != 4 then failwith "Bad magic"
    else
      printf "%02x %02x %02x %02x" (get_byte 0) (get_byte 1) (get_byte 2)
        (get_byte 3) ;
    print_indent indent_level_1 ;
    printf "IconVG Magic Identifier\n"

  let print_metadata vm =
    let chunks, raw_bytes = vm.metadata in
    let raw_stream = Stream.of_list raw_bytes in
    let print_msd msd =
      match msd with
      | ViewBox (c1, c2, c3, c4) ->
          print_quadruple raw_stream (c1, c2, c3, c4)
    in
    let print_chunk chunk =
      let mid_string =
        if chunk.mid = 8 then "Viewbox" else failwith "Unknown mid"
      in
      print_next_decoded raw_stream ;
      print_indent indent_level_1 ;
      printf "Metadata chunk length: %d\n" chunk.length ;
      print_next_decoded raw_stream ;
      print_indent indent_level_1 ;
      printf "Metadata Identifier: %d (%s)\n" chunk.mid mid_string ;
      print_msd chunk.msd
    in
    print_next_decoded raw_stream ;
    print_indent indent_level_1 ;
    printf "Number of metadata chunks: %d\n" (List.length chunks) ;
    List.iter print_chunk chunks

  let print_instructions vm =
    try
      let idx = ref 0 in
      let print_instr_num () = printf "#%04d " !idx in
      while true do
        let instruction, raw_bytes = Vm.next_instruction vm.pc in
        let raw_stream = Stream.of_list raw_bytes in
        let print_geometry_instruction instr =
          let opcode = Stream.next raw_stream in
          let do_regular op_string xs p =
            print_raw_bytes opcode ;
            print_indent indent_level_1 ;
            print_instr_num () ;
            printf "%s (%d reps)\n" op_string (List.length xs) ;
            print_reps p raw_stream xs
          in
          let do_extended op_string xs p =
            print_raw_bytes [List.hd opcode] ;
            print_indent indent_level_1 ;
            print_instr_num () ;
            printf "%s...\n" op_string ;
            print_raw_bytes (List.tl opcode) ;
            print_indent indent_level_2 ;
            printf "...(%d reps)\n" (List.length xs) ;
            print_reps p raw_stream xs
          in
          let p =
            if List.length opcode > 1 then do_extended else do_regular
          in
          match instr with
          | LineTo xs -> p "LineTo" xs print_tuple
          | QuadTo xs -> p "QuadTo" xs print_quadruple
          | CubeTo xs -> p "CubeTo" xs print_sextuple
          | _ -> failwith "unreachable branch"
        in
        let print_instr () =
          match instruction with
          | LineTo _ | QuadTo _ | CubeTo _ ->
              print_geometry_instruction instruction
          | ClosePathMoveTo tup ->
              printf "ClosePath; MoveTo\n" ;
              print_tuple raw_stream tup
          | Parallelogram quad ->
              printf "Parallelogram\n" ;
              print_quadruple raw_stream quad
          | Ellipse (x, quad) ->
              (* TODO Check if quarters is the correct terminology *)
              printf "Ellipse (%d quarters)\n" x ;
              print_quadruple raw_stream quad
          | SelPlus incr -> printf "SEL += %d\n" incr
          | Jump i -> 
              printf "Jump\n";
              print_next_decoded raw_stream;
              print_indent indent_level_2;
              printf "Target: #%04d (PC+%d)\n" (!idx + i + 1) i
          | FDJump (i, features_needed) ->
              printf "Jump Feature-Bits\n";
              print_next_decoded raw_stream;
              print_indent indent_level_2;
              printf "Target: #%04d (PC+%d)\n" (!idx + i + 1) i;
              printf "Features-Bits: %d\n" features_needed
          | LODJump (i, a, b) ->
              printf "Jump Level-of-Detail\n";
              print_next_decoded raw_stream;
              print_indent indent_level_2;
              printf "Target: #%04d (PC+%d)\n" (!idx + i + 1) i;
              print_float raw_stream a;
              print_float raw_stream b
          | Return -> printf "Return\n"
          | SetRegHigh (low4, buf) ->
              printf "Set REGS[SEL+%d].hi32\n" low4 ;
              print_next_decoded raw_stream ;
              print_indent indent_level_2 ;
              printf "hi32 = " ;
              print_hi32_of_int32 buf ;
              print_newline ()
          | SelSetRegs (low4, buf) ->
              let buf_stream = Stream.of_list buf in
              printf "SEL -= %d; Set REGS[SEL+1 .. SEL+%d]\n" (low4 + 2)
                (low4 + 3) ;
              for _i = 0 to low4 + 2 - 1 do
                print_next_decoded raw_stream ;
                print_indent indent_level_2 ;
                printf "lo32 = " ;
                print_lo32 (Stream.next buf_stream) ;
                print_newline () ;
                print_next_decoded raw_stream ;
                print_indent indent_level_2 ;
                printf "hi32 = " ;
                print_hi32 (Stream.next buf_stream) ;
                print_newline ()
              done
          | FillFlat low4 ->
              printf "ClosePath; Fill (flat color) with REGS[SEL+%d]\n" low4
          | FillLinearGradient (low4, nstops, spread, coords) ->
              printf
                "ClosePath; Fill (linear gradient; %s) with REGS[SEL+%d .. \
                 SEL+%d]\n"
                (string_of_spread spread) low4 (low4 + nstops) ;
              print_triple raw_stream coords
          | FillRadialGradient (low4, nstops, spread, coords) ->
              printf
                "ClosePath; Fill (radial gradient; %s) with REGS[SEL+%d .. \
                 SEL+%d]\n"
                (string_of_spread spread) low4 (low4 + nstops) ;
              print_sextuple raw_stream coords
          | Nop -> printf "Nop\n"
          | _ -> printf "disassembly not implemented\n"
        in
        if not (is_geometry_instr instruction) then (
          print_next_decoded raw_stream ;
          print_indent indent_level_1 ;
          print_instr_num () ) ;
        print_instr () ;
        idx := !idx + 1
      done
    with Encdec.Decoder.OutOfOps -> ()

  let run vm = print_magic vm ; print_metadata vm ; print_instructions vm
end
