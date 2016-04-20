open Bap.Std
open Opcode

module Reg(Target : Target) : sig
  val lift : [addx | adcx | subx | sbbx] -> op array -> bil
end
