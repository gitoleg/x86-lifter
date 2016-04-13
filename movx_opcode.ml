open Core_kernel.Std

type movx_rr = [
  | `MOV8rr
  | `MOV8rr_NOREX
  | `MOV16rr
  | `MOV32rr
  | `MOV64rr
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_ri = [
  | `MOV8ri
  | `MOV16ri
  | `MOV32ri
  | `MOV64ri
  | `MOV64ri32
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_mi = [
  | `MOV8mi
  | `MOV16mi
  | `MOV32mi
  | `MOV64mi32
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_rm = [
  | `MOV8rm
  | `MOV8rm_NOREX
  | `MOV16rm
  | `MOV32rm
  | `MOV64rm
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_mr = [
  | `MOV8mr
  | `MOV8mr_NOREX
  | `MOV16mr
  | `MOV32mr
  | `MOV64mr
] [@@deriving bin_io, sexp, compare, enumerate]

type t = [
  | movx_rr
  | movx_ri
  | movx_mi
  | movx_rm
  | movx_mr
] [@@deriving bin_io, sexp, compare, enumerate]
