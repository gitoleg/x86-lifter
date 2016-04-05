open Bap.Std
open Opcode
open X86types

module Reg (CPU : CPU) (Env : Env) : sig
  val lift : btx_reg -> op array -> bil
end
