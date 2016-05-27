open Core_kernel.Std

type t = [
  | `IN8
  | `IN16
  | `IN32
] [@@deriving bin_io, sexp, compare, enumerate]


