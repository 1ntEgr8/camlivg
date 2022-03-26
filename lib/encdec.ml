open Util
open Ty

module Decoder = struct
  exception Invalid_Magic_Identifier

  exception OutOfOps

  exception OpNotImplemented of int

  module type D = sig
    include Cursor.ICursor

    val decode_magic : t -> bytes

    val decode_metadata :
      t -> (metadata_chunk list * raw_decoded_bytes) * int

    val decode_instruction : t -> instruction * raw_decoded_bytes
  end

  module Make (Input : Cursor.ICursor) : D with type t = Input.t = struct
    include Input

    let valid_magic_identifier = Bytes.of_string "\x8AIVG"

    let read_n_bytes hdl n =
      let buf = Bytes.create n in
      Input.read_bytes hdl buf 0 n ;
      buf

    let read_four_bytes hdl = read_n_bytes hdl 4

    let read_eight_bytes hdl = read_n_bytes hdl 8

    let int_list_of_bytes buf =
      List.map
        (fun idx -> int_of_char (Bytes.get buf idx))
        (0 -- (Bytes.length buf - 1))

    let decode_magic hdl =
      let buf = read_four_bytes hdl in
      if valid_magic_identifier <> buf then raise Invalid_Magic_Identifier
      else buf

    let num_encoding_type b =
      let type_spec = b land 0x3 in
      if type_spec = 0 then 4 else if type_spec = 2 then 2 else 1

    let decode_float hdl =
      let n0 = Input.read_byte hdl in
      let n1 = Input.read_byte hdl in
      let n2 = Input.read_byte hdl in
      let n3 = Input.read_byte hdl in
      let n = (n3 lsl 24) lor (n2 lsl 16) lor (n1 lsl 8) lor n0 in
      (Int32.float_of_bits (Int32.of_int n), [[n0; n1; n2; n3]])

    let decode_nat hdl =
      let n0 = Input.read_byte hdl in
      let encoding_type = num_encoding_type n0 in
      let decode_nat_1 () = (n0 lsr 1, [[n0]]) in
      let decode_nat_2 () =
        let n1 = Input.read_byte hdl in
        let result = ((n1 lsl 8) lor n0) lsr 2 in
        (result, [[n0; n1]])
      in
      let decode_nat_4 () =
        let n1 = Input.read_byte hdl in
        let n2 = Input.read_byte hdl in
        let n3 = Input.read_byte hdl in
        let result =
          ((n3 lsl 24) lor (n2 lsl 16) lor (n1 lsl 8) lor n0) lsr 2
        in
        (result, [[n0; n1; n2; n3]])
      in
      let result, raw_bytes =
        if encoding_type = 1 then decode_nat_1 ()
        else if encoding_type = 2 then decode_nat_2 ()
        else decode_nat_4 ()
      in
      (result, encoding_type, raw_bytes)

    let decode_coord hdl =
      let n, encoding_type, raw_bytes = decode_nat hdl in
      let decode_coord_1 () = float_of_int (n - 64) in
      let decode_coord_2 () = float_of_int (n - 8192) /. 64.0 in
      let decode_coord_4 () = Int32.float_of_bits (Int32.of_int (n lsl 2)) in
      let result =
        if encoding_type = 1 then decode_coord_1 ()
        else if encoding_type = 2 then decode_coord_2 ()
        else decode_coord_4 ()
      in
      (result, encoding_type, raw_bytes)

    let decode_viewbox hdl =
      let c1, _, rb1 = decode_coord hdl in
      let c2, _, rb2 = decode_coord hdl in
      let c3, _, rb3 = decode_coord hdl in
      let c4, _, rb4 = decode_coord hdl in
      (ViewBox (c1, c2, c3, c4), rb1 @ rb2 @ rb3 @ rb4)

    let decode_metadata_chunk hdl =
      let chunk_length, _, raw_chunk_length = decode_nat hdl in
      let mid, _, raw_mid = decode_nat hdl in
      let msd, raw_msd =
        if mid = 8 then decode_viewbox hdl else failwith "not implemented"
      in
      ({length= chunk_length; mid; msd}, raw_chunk_length @ raw_mid @ raw_msd)

    let decode_metadata hdl =
      let num_chunks, _, raw_num_chunks = decode_nat hdl in
      let decoded_chunks =
        List.map (fun _ -> decode_metadata_chunk hdl) (1 -- num_chunks)
      in
      let chunks = List.map fst decoded_chunks in
      let raw_bytes =
        raw_num_chunks @ (List.map snd decoded_chunks |> List.concat)
      in
      let start_address = Input.pos hdl in
      ((chunks, raw_bytes), start_address)

    let decode_tuple hdl =
      let c1, _, r1 = decode_coord hdl in
      let c2, _, r2 = decode_coord hdl in
      ((c1, c2), r1 @ r2)

    let decode_triple hdl =
      let c1, _, r1 = decode_coord hdl in
      let c2, _, r2 = decode_coord hdl in
      let c3, _, r3 = decode_coord hdl in
      ((c1, c2, c3), r1 @ r2 @ r3)

    let decode_quadruple hdl =
      let c1, _, r1 = decode_coord hdl in
      let c2, _, r2 = decode_coord hdl in
      let c3, _, r3 = decode_coord hdl in
      let c4, _, r4 = decode_coord hdl in
      ((c1, c2, c3, c4), r1 @ r2 @ r3 @ r4)

    let decode_sextuple hdl =
      let c1, _, r1 = decode_coord hdl in
      let c2, _, r2 = decode_coord hdl in
      let c3, _, r3 = decode_coord hdl in
      let c4, _, r4 = decode_coord hdl in
      let c5, _, r5 = decode_coord hdl in
      let c6, _, r6 = decode_coord hdl in
      ((c1, c2, c3, c4, c5, c6), r1 @ r2 @ r3 @ r4 @ r5 @ r6)

    let decode_extra_data hdl =
      let (ed_length, _, raw_ed_length) = decode_nat hdl in
      let ed_bytes = read_n_bytes hdl ed_length in
      ( (ed_length, ed_bytes)
      , raw_ed_length @ [int_list_of_bytes ed_bytes])
      
    let decode_segref hdl =
      let buf = read_eight_bytes hdl in
      let is_inline = Int32.equal (Bytes.get_int32_le buf 32) Int32.zero in
      let decode_inline_segref () =
        let segment_type = Bytes.get_uint8 buf 0 in
        let segment_length = 
          let sub = Bytes.sub buf 1 3 in
          let ext = Bytes.extend sub 0 5 in 
          Bytes.fill ext 3 5 '\x00';
          Bytes.get_int64_le ext 0
        in
        let segment_offset = Int64.of_int (Input.pos hdl) in
        SegrefInline (segment_type, segment_length, segment_offset)
      in
      let decode_absolute_segref () =
        let is_direct = ((int_of_char (Bytes.get buf 7)) land 0x1) = 0 in
        if is_direct then
          let segment_type = Bytes.get_uint8 buf 0 in
          let segment_length =
            let sub = Bytes.sub buf 1 3 in
            let ext = Bytes.extend sub 0 5 in
            Bytes.fill ext 3 5 '\x00';
            Bytes.get_int64_le ext 0
          in
          let segment_offset =
            let sub = Bytes.sub buf 4 4 in
            Int64.of_int32 (Bytes.get_int32_le sub 0)
          in
          SegrefAbsoluteDirect (segment_type, segment_length, segment_offset)
        else
          let segment_type = Bytes.get_uint8 buf 0 in
          let indirect_offset =
            let sub = Bytes.sub buf 1 7 in
            let ext = Bytes.extend sub 0 1 in
            Bytes.fill ext 7 1 '\x00';
            Bytes.get_int64_le ext 0
          in
          SegrefAbsoluteIndirect (segment_type, indirect_offset)
      in
      let segref = 
        if is_inline then
          decode_inline_segref ()
        else
          decode_absolute_segref ()
      in
      (segref, [int_list_of_bytes buf])

    let decode_instruction hdl =
      let opcode =
        try Input.read_byte hdl with End_of_file -> raise OutOfOps
      in
      let low4 = opcode land 0xF in
      let get_repcount () =
        if low4 != 0 then (low4, [])
        else
          let n, _, rn = decode_nat hdl in
          (n + 16, List.concat rn)
      in
      let instr, raw_bytes =
        if opcode >= 0x00 && opcode <= 0x0F then
          let reps, raw_reps = get_repcount () in
          let coords, raw_bytes =
            repeat_and_split (fun _ -> decode_tuple hdl) reps
          in
          (LineTo coords, (opcode :: raw_reps) :: List.concat raw_bytes)
        else if opcode >= 0x10 && opcode <= 0x1F then
          let reps, raw_reps = get_repcount () in
          let coords, raw_bytes =
            repeat_and_split (fun _ -> decode_quadruple hdl) reps
          in
          (QuadTo coords, (opcode :: raw_reps) :: List.concat raw_bytes)
        else if opcode >= 0x20 && opcode <= 0x2F then
          let reps, raw_reps = get_repcount () in
          let coords, raw_bytes =
            repeat_and_split (fun _ -> decode_sextuple hdl) reps
          in
          (CubeTo coords, (opcode :: raw_reps) :: List.concat raw_bytes)
        else if opcode >= 0x30 && opcode <= 0x34 then
          let quad, raw_bytes = decode_quadruple hdl in
          if opcode == 0x34 then (Parallelogram quad, [opcode] :: raw_bytes)
          else (Ellipse ((opcode land 0xF) + 1, quad), [opcode] :: raw_bytes)
        else if opcode == 0x35 then
          let c1, _, r1 = decode_coord hdl in
          let c2, _, r2 = decode_coord hdl in
          (ClosePathMoveTo (c1, c2), ([opcode] :: r1) @ r2)
        else if opcode == 0x36 then
          let b = Input.read_byte hdl in
          (SelPlus b, [[opcode]; [b]])
        else if opcode == 0x37 then (Nop, [[opcode]])
        else if opcode == 0x38 then
          let jump_count, _, rjump_count = decode_nat hdl in
          (Jump jump_count, [opcode] :: rjump_count)
        else if opcode == 0x39 then
          let jump_count, _, rjump_count = decode_nat hdl in
          let features_needed, _, rfeatures_needed = decode_nat hdl in
          ( FDJump (jump_count, features_needed)
          , ([opcode] :: rjump_count) @ rfeatures_needed )
        else if opcode == 0x3A then
          let jump_count, _, rjump_count = decode_nat hdl in
          let lod0, rlod0 = decode_float hdl in
          let lod1, rlod1 = decode_float hdl in
          ( LODJump (jump_count, lod0, lod1)
          , ([opcode] :: rjump_count) @ rlod0 @ rlod1 )
        else if opcode == 0x3B then (Return, [[opcode]])
        else if opcode == 0x3C then
          let (segref, raw_segref) = decode_segref hdl in
          ( CallUntransformed segref
          , ([opcode] :: raw_segref))
        else if opcode == 0x3D then
          let alpha_value = Input.read_byte hdl in
          let (matrix, raw_matrix) = decode_sextuple hdl in
          let (segref, raw_segref) = decode_segref hdl in
          ( CallTransformed (alpha_value, matrix, segref)
          , ([opcode] :: [alpha_value] :: raw_matrix @ raw_segref))
        else if opcode >= 0x3E && opcode <= 0x3F then
          let (extra_data, raw_extra_data) = decode_extra_data hdl in
          ( Reserved0 extra_data
          , ([opcode] :: raw_extra_data))
        else if opcode >= 0xB0 && opcode <= 0xBF then
          let (extra_data, raw_extra_data) = decode_extra_data hdl in
          ( Reserved1 extra_data
          , ([opcode] :: raw_extra_data))
        else if opcode >= 0xC0 && opcode <= 0xDF then
          let (extra_data, raw_extra_data) = decode_extra_data hdl in
          let c1, _, r1 = decode_coord hdl in
          let c2, _, r2 = decode_coord hdl in
          ( Reserved2 (extra_data, (c1, c2))
          , ([opcode] :: raw_extra_data @ r1 @ r2))
        else if opcode >= 0xE0 && opcode <= 0xFF then
          let (extra_data, raw_extra_data) = decode_extra_data hdl in
          ( Reserved3 extra_data
          , ([opcode] :: raw_extra_data))
        else if opcode >= 0x40 && opcode <= 0x4F then
          let buf = read_four_bytes hdl in
          ( SetRegLow (low4, Bytes.get_int32_le buf 0)
          , [[opcode]; int_list_of_bytes buf] )
        else if opcode >= 0x50 && opcode <= 0x5F then
          let buf = read_four_bytes hdl in
          ( SetRegHigh (low4, Bytes.get_int32_le buf 0)
          , [[opcode]; int_list_of_bytes buf] )
        else if opcode >= 0x60 && opcode <= 0x6F then
          let buf = read_eight_bytes hdl in
          ( SetReg (low4, Bytes.get_int64_le buf 0)
          , [[opcode]; int_list_of_bytes buf] )
        else if opcode >= 0x70 && opcode <= 0x7F then
          let buf =
            List.map
              (fun _ -> int_list_of_bytes (read_four_bytes hdl))
              (1 -- (2 * (low4 + 2)))
          in
          (SelSetRegs (low4, buf), [opcode] :: buf)
        else if opcode >= 0x80 && opcode <= 0x8F then
          (FillFlat low4, [[opcode]])
        else if opcode >= 0x90 && opcode <= 0x9F then
          let gradient_config = Input.read_byte hdl in
          let nstops = (gradient_config land 0x3F) + 2 in
          let spread = gradient_config lsr 6 in
          let coords, raw_bytes = decode_triple hdl in
          ( FillLinearGradient (low4, nstops, spread, coords)
          , [opcode; gradient_config] :: raw_bytes )
        else if opcode >= 0xA0 && opcode <= 0xAF then
          let gradient_config = Input.read_byte hdl in
          let nstops = (gradient_config land 0x3F) + 2 in
          let spread = gradient_config lsr 6 in
          let coords, raw_bytes = decode_sextuple hdl in
          ( FillRadialGradient (low4, nstops, spread, coords)
          , [opcode; gradient_config] :: raw_bytes )
        else raise (OpNotImplemented opcode)
      in
      (instr, raw_bytes)
  end
end
