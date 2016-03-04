open Bap.Std
open Opcode

module Reg(Target : Target) : sig
  val lift : xadd_reg -> op array -> bil
end
