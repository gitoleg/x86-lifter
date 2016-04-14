open Bap.Std

module Reg(Target : Target) : sig
  val lift : Opcode.addx -> op array -> bil
end
