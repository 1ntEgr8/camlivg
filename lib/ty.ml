open Util

type raw_decoded_bytes = int list list

type extra_data = int * bytes

type segref =
  (* SegrefInline (type, length, offset) *)
  | SegrefInline of (int * int64 * int64)
  (* SegrefAbsoluteDirect (type, length, offset) *)
  | SegrefAbsoluteDirect of (int * int64 * int64)
  (* SegrefAbsoluteIndirect (type, indirect_offset) *)
  | SegrefAbsoluteIndirect of (int * int64)

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
  (* CallTransformed (alpha_value, affine_matrix, segref) *)
  | CallTransformed of int * sextuple * segref
  | CallUntransformed of segref
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
  | Reserved0 of extra_data
  | Reserved1 of extra_data
  | Reserved2 of extra_data * tuple
  | Reserved3 of extra_data
  | Nop

type msd = ViewBox of float * float * float * float

type metadata_chunk = {length: int; mid: int; msd: msd}
