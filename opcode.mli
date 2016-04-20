open Core_kernel.Std
open Bap.Std
open Regular.Std

type btx = Btx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type movx = Movx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type t = [
  | btx
  | movx
] [@@deriving bin_io, sexp, compare, enumerate]

include Regular with type t := t

val decode : Disasm_expert.Basic.full_insn -> t option


