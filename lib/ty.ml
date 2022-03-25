open Util

type raw_decoded_bytes = int list list

type instruction =
  | LineTo of tuple list
  | QuadTo of quadruple list
  | CubeTo of sextuple list
  | ClosePathMoveTo of tuple
  | Parallelogram of quadruple
  | Ellipse of int * quadruple
  | SelPlus of int
  | Jump of int
  | FDJump of int * int
  | LODJump of int * float * float
  | CallUntransformed of int * sextuple
  | Return
  (* SetRegLow (low4, data) *)
  | SetRegLow of int * int32
  (* SetRegHigh (low4, data) *)
  | SetRegHigh of int * int32
  (* SetReg (low4, data) *)
  | SetReg of int * int64
  (* SelSetRegs (low4, data) *)
  | SelSetRegs of int * raw_decoded_bytes
  (* FillFlat (low4) *)
  | FillFlat of int
  (* FillLinearGradient (low4, nstops, spread, coords) *)
  | FillLinearGradient of int * int * int * triple
  (* FillRadientGradient (low4, nstops, spread, coords) *)
  | FillRadialGradient of int * int * int * sextuple
  | Reserved of bytes
  | Nop

type segref = Inline | AbsoluteDirect | AbsoluteIndirect

type msd = ViewBox of float * float * float * float

type metadata_chunk = {length: int; mid: int; msd: msd}
