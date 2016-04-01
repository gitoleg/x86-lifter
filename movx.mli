open Bap.Std
open Opcode
open X86types

exception Mov_error of string

module Reg (Env : Env) : sig
  val lift : movx -> op array -> bil
end
