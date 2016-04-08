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

module IA32 : S
module AMD64 : S
