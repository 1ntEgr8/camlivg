open Printf
open Camlivg
module IvgDecoder = Decoder.Make (Cursor.IFile)
module IvgVm = Vm.Make (IvgDecoder)
module IvgDisassembler = Disassembler.Make (IvgVm)
module IvgCairoRenderer = Cairo_renderer.Make (IvgVm)

let usage_msg = "camlivg [-d] <file1> [<file2>] ..."

let disassemble = ref false

let output = ref ""

let input_files = ref []

let anon_fun filename = input_files := filename :: !input_files

let supported_backends = ["png"]

let speclist =
  [ ("-d", Arg.Set disassemble, "Disassemble")
  ; ( "-o"
    , Arg.Set_string output
    , "Set output image format (supported formats: png)" ) ]

let () =
  Arg.parse speclist anon_fun usage_msg ;
  if List.length !input_files = 0 then (
    printf "Missing input files\n" ;
    printf "%s\n" usage_msg )
  else
    let use_backend =
      if String.length !output = 0 then false
      else
        let found =
          List.find_opt (fun x -> !output = x) supported_backends
        in
        match found with
        | Some _ -> true
        | None ->
            printf "Unsupported backend %s\n" !output ;
            exit (-1)
    in
    List.iter
      (fun filename ->
        try
          let hdl = open_in_bin filename in
          let vm = IvgVm.init hdl in
          if !disassemble then IvgDisassembler.run vm ;
          if use_backend then IvgCairoRenderer.run vm
        with Decoder.Invalid_Magic_Identifier -> printf "bad magic" )
      !input_files
