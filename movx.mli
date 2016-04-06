open Bap.Std
open Opcode
open X86types

exception Invalid_signature
exception Invalid_operands

module Reg (CPU : CPU) (Env : Env) : sig
  val lift : movx -> op array -> bil
end
