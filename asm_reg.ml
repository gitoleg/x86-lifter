open Core_kernel.Std
open Bap.Std

include Asm_reg_types

let width = function
  | #r8 -> `r8
  | #r16 -> `r16
  | #r32 -> `r32
  | #r64 -> `r64

let bitwidth r = width r |> Size.in_bits

type spec = [`Nil | t] [@@deriving sexp]

let decode reg =
  match Reg.name reg |> Sexp.of_string |> spec_of_sexp with
  | `Nil -> None
  | #t as r -> Some r
  | exception exn -> Error.failwiths "unknown register"
                       reg Reg.sexp_of_t
