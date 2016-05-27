open Core_kernel.Std
open Bap.Std
open Regular.Std

include module type of Opcode_types

include Regular with type t := t

val decode : Disasm_expert.Basic.full_insn -> t option
