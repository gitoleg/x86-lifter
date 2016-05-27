open Core_kernel.Std
open Bap.Std
open Regular.Std

type btx = Btx_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type cmps = Cmps_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type cmpxchg = Cmpxchg_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type ins = Ins_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type lods = Lods_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type mov = Mov_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type movs = Movs_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type outs = Outs_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type scas = Scas_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type stos = Stos_opcode.t [@@deriving bin_io, sexp_poly, compare, enumerate]
type t = [
  | btx
  | cmps
  | cmpxchg
  | ins
  | lods
  | mov
  | movs
  | outs
  | scas
  | stos
] [@@deriving bin_io, sexp, compare, enumerate]
