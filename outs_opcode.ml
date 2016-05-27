open Core_kernel.Std

type t = [
  | `OUTSB
  | `OUTSW
  | `OUTSD
] [@@deriving bin_io, sexp, compare, enumerate]
