open Core_kernel.Std

type t = [
  | `MOVSB
  | `MOVSW
  | `MOVSD
  | `MOVSQ
] [@@deriving bin_io, sexp, compare, enumerate]
