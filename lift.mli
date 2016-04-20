open Core_kernel.Std
open Bap.Std

type lifter =
  (mem * Disasm_expert.Basic.full_insn) list ->
  bil Or_error.t list

val ia32 : lifter
val amd64 : lifter 
