open Core_kernel.Std
open Bap.Std

module type S = sig
  val register : Opcode.t ->
    (op array -> bil) -> unit Or_error.t

  val register_all :
    Opcode.t list ->
    (Opcode.t -> op array -> bil) ->
    unit Or_error.t

  val lift : Opcode.t -> op array -> bil Or_error.t
end

module type R = sig
  val register: unit -> unit Or_error.t
end

let create () = Opcode.Table.create ~size:(List.length Opcode.all) ()
let register t op lift =
  match Opcode.Table.add t ~key:op ~data:lift with
  | `Ok -> Result.ok_unit
  | `Duplicate ->
    Or_error.error "dupplicate opcode" op Opcode.sexp_of_t

let lift t op ops =
  match Opcode.Table.find t op with
  | Some lift -> Or_error.try_with (fun () -> lift op ops)
  | None ->
    Or_error.error "unsupported operation code" op Opcode.sexp_of_t

let register_all t op lift =
  List.map op ~f:(fun op -> register t op lift) |>
  Or_error.combine_errors_unit
                   
module type Data = sig
  val all : (Opcode.t -> op array -> bil) Opcode.Table.t
end

module Make (D : Data) : S = struct
  let register op lift = register D.all op (fun _ -> lift)
  let register_all = register_all D.all
  let lift = lift D.all
end

module IA32 = Make(struct let all = create () end)
module AMD64 = Make(struct let all = create () end)
