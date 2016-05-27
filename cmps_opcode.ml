open Core_kernel.Std

type t = [
  | `CMPS8
  | `CMPS16
  | `CMPS32
  | `CMPS64
] [@@deriving bin_io, sexp, compare, enumerate]

