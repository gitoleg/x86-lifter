open Core_kernel.Std
open Bap.Std
open Regular.Std

type addx32 = Addx_opcode.t32
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type addx64 = Addx_opcode.t64
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type btx = Btx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type movx = Movx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type t = [
  | addx32
  | addx64
  | btx
  | movx
] [@@deriving bin_io, sexp, compare, enumerate]

include Regular with type t := t

val decode : Disasm_expert.Basic.full_insn -> t option


