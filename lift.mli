open Core_kernel.Std
open Bap.Std

type x86_lifter =
  (mem * Disasm_expert.Basic.full_insn) list ->
  bil Or_error.t list

val ia32 : ?on_unsupported:lifter -> x86_lifter
val amd64 : ?on_unsupported:lifter -> x86_lifter 
