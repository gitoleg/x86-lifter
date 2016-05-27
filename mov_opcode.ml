open Core_kernel.Std

type mov_rr_ia32 = [
  | `MOV8rr
  | `MOV8rr_NOREX
  | `MOV8rr_REV
  | `MOV16rr
  | `MOV32rr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rr = [
  | mov_rr_ia32
  | `MOV64rr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ri_ia32 = [
  | `MOV8ri
  | `MOV16ri
  | `MOV32ri
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ri = [
  | mov_ri_ia32
  | `MOV64ri
  | `MOV64ri32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mi_ia32 = [
  | `MOV8mi
  | `MOV16mi
  | `MOV32mi
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mi = [
  | mov_mi_ia32
  | `MOV64mi32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rm_ia32 = [
  | `MOV8rm
  | `MOV8rm_NOREX
  | `MOV16rm
  | `MOV32rm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rm = [
  | mov_rm_ia32
  | `MOV64rm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mr_ia32 = [
  | `MOV8mr
  | `MOV8mr_NOREX
  | `MOV16mr
  | `MOV32mr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mr = [
  | mov_mr_ia32
  | `MOV64mr
] [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-32 mode *)
type mov_oa_ia32 = [
  | `MOV8o8a
  | `MOV16o16a
  | `MOV32o32a
] [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-64 mode *)
type mov_oa_amd64 = [
  | `MOV64o8a
  | `MOV64o16a
  | `MOV64o32a
  | `MOV64o64a
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_oa = [mov_oa_ia32 | mov_oa_amd64]
  [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-32 mode *)
type mov_ao_ia32 = [
  | `MOV8ao8
  | `MOV16ao16
  | `MOV32ao32
] [@@deriving bin_io, sexp, compare, enumerate]

(** valid only in x86-64 mode *)
type mov_ao_amd64 = [
  | `MOV64ao8
  | `MOV64ao16
  | `MOV64ao32
  | `MOV64ao64
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ao = [mov_ao_ia32 | mov_ao_amd64]
 [@@deriving bin_io, sexp, compare, enumerate]

type mov_ia32 = [
  | mov_rr_ia32
  | mov_ri_ia32
  | mov_mi_ia32
  | mov_rm_ia32
  | mov_mr_ia32
  | mov_oa_ia32
  | mov_ao_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_amd64 = [
  | mov_rr
  | mov_ri
  | mov_mi
  | mov_rm
  | mov_mr
  | mov_oa_amd64
  | mov_ao_amd64
] [@@deriving bin_io, sexp, compare, enumerate]

type t = [
  | mov_ia32
  | mov_amd64
] [@@deriving bin_io, sexp, compare, enumerate]
