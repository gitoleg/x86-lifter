open Bap.Std
open Opcode
open X86types

module Reg(Target : Target)(Env : Env) : sig
  val lift : btx_reg -> op array -> bil
end
