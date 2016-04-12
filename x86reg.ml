open Core_kernel.Std
open Bap.Std

(** 8-bit low byte GPR registers *)
type r8l = [
  | `AL | `BL | `CL | `DL
  | `SIL | `DIL | `BPL | `SPL
  | `R8B | `R9B | `R10B | `R11B
  | `R12B | `R13B | `R14B | `R15B
] [@@deriving sexp]

(** 8-bit high-byte GPR registers *)
type r8h = [`AH | `BH | `CH | `DH ] [@@deriving sexp]

(** all 8 bit GPR registers *)
type r8 = [r8l | r8h] [@@deriving sexp]

(** 16-bit GPR registers *)
type r16 = [
  |`AX | `BX | `CX | `DX
  | `DI | `SI | `BP | `SP
  | `R8W | `R9W | `R10W | `R11W
  | `R12W | `R13W | `R14W | `R15W
] [@@deriving sexp]

(** 32-bit GPR registers *)
type r32 = [
  | `EAX | `EBX | `ECX | `EDX
  | `EDI | `ESI | `EBP | `ESP
  | `R8D | `R9D | `R10D | `R11D
  | `R12D | `R13D | `R14D | `R15D
] [@@deriving sexp]

(** 64-bit GPR registers *)
type r64 = [
  | `RAX | `RBX | `RCX | `RDX
  | `RDI | `RSI | `RBP | `RSP
  | `R8 | `R9 | `R10 | `R11
  | `R12 | `R13 | `R14 | `R15
] [@@deriving sexp]

type t = [
  | r8
  | r16
  | r32
  | r64
] [@@deriving sexp]

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
