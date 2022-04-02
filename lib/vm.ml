open Ty

type vm_status = Ready | Success | Failure

type 'a vm =
  { magic: bytes
  ; metadata: metadata_chunk list * raw_decoded_bytes
  ; suggested_palette: int array option
  ; pc: 'a
  ; status: vm_status
  ; start_address: int }

module type V = sig
  type t

  val init : t -> t vm

  val jump_to : t -> int -> unit

  val next_instruction : t -> instruction * raw_decoded_bytes

  val reset : t vm -> unit
end

module Make (Decoder : Encdec.Decoder.D) : V with type t = Decoder.t = struct
  type t = Decoder.t

  let init hdl =
    let magic = Decoder.decode_magic hdl in
    let metadata, start_address = Decoder.decode_metadata hdl in
    let suggested_palette_chunk =
      List.find_opt
        (fun m -> match m.msd with SuggestedPalette _ -> true | _ -> false)
        (fst metadata)
    in
    let suggested_palette =
      match suggested_palette_chunk with
      | Some chunk -> (
        match chunk.msd with
        | SuggestedPalette p -> Some p
        | _ -> failwith "unreachable: vm suggested palette" )
      | _ -> None
    in
    { magic
    ; metadata
    ; pc= hdl
    ; status= Ready
    ; start_address
    ; suggested_palette }

  let jump_to hdl offset = Decoder.seek hdl offset

  let next_instruction hdl = Decoder.decode_instruction hdl

  let reset vm = Decoder.seek vm.pc vm.start_address
end
