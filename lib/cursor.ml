module type ICursor = sig
  (* include Stream.IStream *)
  type t

  val read_byte : t -> int

  val read_bytes : t -> bytes -> int -> int -> unit

  val pos : t -> int

  val seek : t -> int -> unit

  val reset : t -> unit
end

module IFile : ICursor with type t = in_channel = struct
  type t = in_channel

  let read_byte = input_byte

  let read_bytes = really_input

  let pos = pos_in

  let seek = seek_in

  let reset hdl = seek hdl 0
end
