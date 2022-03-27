open Printf
open Camlivg
module IvgDecoder = Decoder.Make (Cursor.IFile)
module IvgVm = Vm.Make (IvgDecoder)
module IvgDisassembler = Disassembler.Make (IvgVm)
module IvgCairoRenderer = Cairo_renderer.Make (IvgVm)

let () =
  let hdl = open_in_bin "test/data/gradient.iconvg" in
  try
    let vm = IvgVm.init hdl in
    IvgDisassembler.run vm ; IvgCairoRenderer.run vm
  with Decoder.Invalid_Magic_Identifier -> printf "bad magic"
