open Core_kernel.Std
open Bap.Std
open Regular.Std

type t = [
  | `LOCK_PREFIX
  | `REX64_PREFIX
  | `DATA16_PREFIX
  | `REP_PREFIX
  | `REPNE_PREFIX
] [@@deriving sexp, compare, enumerate]

include Regular with type t := t

val decode : Disasm_expert.Basic.full_insn -> t option
