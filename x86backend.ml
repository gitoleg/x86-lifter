open Core_kernel.Std
open Bap.Std

module type S = sig
  val register : Opcode.t ->
    (Opcode.t -> op array -> bil) -> [`Ok | `Duplicate ]

  val register_all :
    Opcode.t list ->
    (Opcode.t -> op array -> bil) ->
    unit Or_error.t

  val lift : Opcode.t -> op array -> bil Or_error.t
end

module Table = Hashtbl.Make(
  struct
    type t = Opcode.t [@@deriving sexp, compare]
    let hash = Hashtbl.hash
  end)

let create () = Table.create ~size:(List.length Opcode.all) ()
let register t op lift = Table.add t ~key:op ~data:lift

let lift t op ops =
  match Table.find t op with
  | Some lift -> Or_error.try_with (fun () -> lift op ops)
  | None ->
    Or_error.error "unsupported operation code" op Opcode.sexp_of_t

let register_all t op lift =
  List.map op ~f:(fun op ->
      match register t op lift with
      | `Ok -> Result.ok_unit
      | `Duplicate ->
        Or_error.error "dupplicate" op Opcode.sexp_of_t) |>
  Or_error.combine_errors_unit
                   
module type Data = sig
  val all : (Opcode.t -> op array -> bil) Table.t
end

module Make (D : Data) : S = struct
  let register = register D.all
  let register_all = register_all D.all
  let lift = lift D.all
end

module IA32 = Make(struct let all = create () end)
module AMD64 = Make(struct let all = create () end)
