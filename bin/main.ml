open Printf
open Camlivg
module IvgDecoder = Decoder.Make (Cursor.IFile)
module IvgVm = Vm.Make (IvgDecoder)
module IvgDisassembler = Disassembler.Make (IvgVm)

let () =
  let hdl = open_in_bin "test/data/lod-polygon.iconvg" in
  try
    let vm = IvgVm.init hdl in
    IvgDisassembler.run vm
  with Decoder.Invalid_Magic_Identifier -> printf "bad magic"
