open Ty

type vm_status = Ready | Success | Failure

type 'a vm =
  { magic: bytes
  ; metadata: metadata_chunk list * raw_decoded_bytes
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
    {magic; metadata; pc= hdl; status= Ready; start_address}

  let jump_to hdl offset = Decoder.seek hdl offset

  let next_instruction hdl = Decoder.decode_instruction hdl

  let reset vm = Decoder.seek vm.pc vm.start_address
end
