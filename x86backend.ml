open Core_kernel.Std
open Bap.Std

include X86backend_types

module type Data = sig
  val data : (mem -> op array -> bil Or_error.t) Opcode.Table.t
end

module Make (D : Data) : X86backend_types.S = struct
  open D
  let register op lift =
    Opcode.Table.set data ~key:op ~data:lift
  let lift op =
    Opcode.Table.find data op |>
    Option.value ~default:(fun (_:mem) (_:op array) ->
        Or_error.error "unsupperted operation code"
          op Opcode.sexp_of_t)
end

let data = Opcode.Table.create ~size:(List.length Opcode.all)

module IA32 = Make(struct let data = data () end)
module AMD64 = Make(struct let data = data () end)
