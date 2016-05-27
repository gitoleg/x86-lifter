open Core_kernel.Std
open Bap.Std

include X86backend_types

module type Data = sig
  val data : (mem -> op array -> bil Or_error.t) Opcode.Table.t
end

module Make (D : Data) : S = struct
  open D
  let register op lift =
    Opcode.Table.set data ~key:op ~data:lift
  let lift op mem ops =
    Opcode.Table.find_and_call data op
      ~if_found:(fun lift ->
          Or_error.try_with_join (fun () -> lift mem ops))
      ~if_not_found:(fun op -> Or_error.error
                        "unsupported operation code"
                        op Opcode.sexp_of_t)
end

let data = Opcode.Table.create ~size:(List.length Opcode.all)

module IA32 = Make(struct let data = data () end)
module AMD64 = Make(struct let data = data () end)
