open Core_kernel.Std
open Bap.Std
open Regular.Std

type t = [
  | `LOCK_PREFIX
  | `REX64_PREFIX
  | `DATA16_PREFIX
  | `REP_PREFIX
  | `REPNE_PREFIX
] [@@deriving bin_io, sexp, compare, enumerate]

module T = struct
  open Format

  type nonrec t = t [@@deriving bin_io, compare, sexp]
  let module_name = Some "Decode"
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
