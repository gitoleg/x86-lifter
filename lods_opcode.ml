open Core_kernel.Std

type t = [
  | `LODSB
  | `LODSW
  | `LODSD
  | `LODSQ
] [@@deriving bin_io, sexp, compare, enumerate]
