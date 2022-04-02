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
  | Ellipse of int * quadruple (* num_quarters, quad *)
  | SelPlus of int
  | Jump of int
  | FDJump of int * int
  | LODJump of int * float * float (* jump_count, lod0, lod1 *)
  | CallTransformed of int * sextuple * segref (* alpha_value, affine_matrix, segref *)
  | CallUntransformed of segref
  | Return
  | SetRegLow of int * int32 (* low4, data *)
  | SetRegHigh of int * int32 (* low4, data *)
  | SetReg of int * int64 (* low4, data *)
  | SelSetRegs of int * raw_decoded_bytes (* low4, data *)
  | FillFlat of int (* low4 *)
  | FillLinearGradient of int * int * int * triple (* low4, nstops, spread, ngm *)
  | FillRadialGradient of int * int * int * sextuple (* low4, nstops, spread, ngm *)
  | Reserved0 of extra_data
  | Reserved1 of extra_data
  | Reserved2 of extra_data * tuple
  | Reserved3 of extra_data
  | Nop

type msd =
  | ViewBox of float * float * float * float
  | SuggestedPalette of int array

(* rgba colors *)

type metadata_chunk = {length: int; mid: int; msd: msd}
