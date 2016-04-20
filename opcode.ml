open Core_kernel.Std
open Bap.Std
open Regular.Std

type addx32 = Addx_opcode.t32
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type addx64 = Addx_opcode.t64
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type btx = Btx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type movx = Movx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type t = [
  | addx32
  | addx64
  | btx
  | movx
] [@@deriving bin_io, sexp, compare, enumerate]

module T = struct
  open Format

  type nonrec t = t [@@deriving bin_io, compare, sexp]
  let module_name = Some "Opcode"
  let version = "0.1"
  let pp fmt t =
    fprintf fmt "%s" (sexp_of_t t |> Sexp.to_string)

  let hash = Hashtbl.hash
end

include Regular.Make(T)

let decode insn =
  Option.try_with (fun () ->
    Disasm_expert.Basic.Insn.name insn |>
    Sexp.of_string |>
    t_of_sexp)

