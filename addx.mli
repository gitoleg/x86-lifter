open Bap.Std

module Reg(Target : Target) : sig
  val lift : Opcode.addx -> op array -> bil
end

module RegCarry(Target : Target) : sig
  val lift : Opcode.adcx -> op array -> bil
end
