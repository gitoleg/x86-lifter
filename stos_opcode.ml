open Core_kernel.Std

type t = [
  | `STOSB
  | `STOSW
  | `STOSD
  | `STOSQ
] [@@deriving bin_io, sexp, compare, enumerate]
