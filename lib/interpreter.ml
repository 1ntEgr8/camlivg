open Vm

module type I = sig
  type t

  val run : t vm -> unit
end

(* module Disassembler : I = struct (* open Printf

   let print_tuple (c1, c2) = printf "(%+g, %+g)\n" c1 c2

   let print_quadruple (c1, c2, c3, c4) = printf "(%+g, %+g, %+g, %+g)\n" c1
   c2 c3 c4 *)

   let run = ()

   (* let fold vm () = (* TODO Change vm instrs from list to array *) let
   instr = List.nth vm.instrs vm.pc in let f () = match instr with | LineTo
   coords -> printf "LineTo\n"; List.iter (fun tup -> printf "(rep)\n";
   print_tuple tup ) coords | ClosePathMoveTo tup -> printf
   "ClosePathMoveTo\n"; print_tuple tup | Ellipse (num_segments, quad) ->
   printf "Ellipse (%d segments)\n" num_segments; print_quadruple quad |
   Parallelogram quad -> printf "Parallelogram\n"; print_quadruple quad |
   SelPlus v -> printf "SEL += %d (mod 64)\n" v | Nop -> printf "NOP" | _ ->
   failwith "not implemented in main" in let vm_status = if vm.pc ==
   (List.length vm.instrs) - 1 then Success else Ready in f (); ({ vm with pc
   = vm.pc + 1; status = vm_status }, ()) *) end *)
