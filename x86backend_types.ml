open Core_kernel.Std
open Bap.Std


module type S = sig
  val register : Opcode.t -> (mem -> op array -> bil Or_error.t) -> unit
  val lift : Opcode.t -> mem -> op array -> bil Or_error.t
end

module type R = sig
  val register: unit -> unit
end