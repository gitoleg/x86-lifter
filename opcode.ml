open Core_kernel.Std
open Regular.Std


type btx = Btx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type movx = Movx_opcode.t
  [@@deriving bin_io, sexp_poly, compare, enumerate]

type t = [
  | btx
  | movx
] [@@deriving bin_io, sexp, compare, enumerate]

type prefix = [
  | `LOCK_PREFIX
  | `REX64_PREFIX
  | `DATA16_PREFIX
  | `REP_PREFIX
  | `REPNE_PREFIX
] [@@deriving sexp, compare, enumerate]

module T = struct
  open Format

  type nonrec t = t [@@deriving bin_io, compare, sexp]
  let module_name = Some "X86lifter.Opcode"
  let version = "0.1"
  let pp fmt t =
    fprintf fmt "%s" (sexp_of_t t |> Sexp.to_string)

  let hash = Hashtbl.hash
end

include Regular.Make(T)

