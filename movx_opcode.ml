open Core_kernel.Std

type movx_rr = [
  | `MOV8rr
  | `MOV8rr_NOREX
  | `MOV8rr_REV
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

(** valid only in x86-32 mode *)
type movx_oa_ia32 = [
  | `MOV8o8a
  | `MOV16o16a
  | `MOV32o32a
] [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-64 mode *)
type movx_oa_amd64 = [
  | `MOV64o8a
  | `MOV64o16a
  | `MOV64o32a
  | `MOV64o64a
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_oa = [movx_oa_ia32 | movx_oa_amd64]
  [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-32 mode *)
type movx_ao_ia32 = [
  | `MOV8ao8
  | `MOV16ao16
  | `MOV32ao32
] [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-64 mode *)
type movx_ao_amd64 = [
  | `MOV64ao8
  | `MOV64ao16
  | `MOV64ao32
  | `MOV64ao64
] [@@deriving bin_io, sexp, compare, enumerate]

type movx_ao = [movx_ao_ia32 | movx_ao_amd64]
 [@@deriving bin_io, sexp, compare, enumerate]

type t = [
  | movx_rr
  | movx_ri
  | movx_mi
  | movx_rm
  | movx_mr
  | movx_oa
  | movx_ao
] [@@deriving bin_io, sexp, compare, enumerate]
