open Core_kernel.Std
open Bap.Std

module type S = sig
  (** Register representation *)
  module RR : sig
    type t
    val of_reg : Operand.reg -> t option
    val of_reg_exn : Operand.reg -> t
    val to_x86reg : t -> X86reg.t
    val width : t -> [`r8 | `r16 | `r32 | `r64]
    val var : t -> var
    val size : [`r32 | `r64]
    val get : t -> exp
    val set : t -> exp -> stmt
  end

  (**Imm model*)
  module IM : sig
    type t
    val of_imm : Operand.imm -> t
    val get : width:([`r8 | `r16 | `r32 | `r64]) -> t -> exp
  end

  (** Memory model *)
  module MM : sig
    type t
    val of_mem : Operand.mem -> t
    val of_offset : Operand.imm -> t
    val addr : t -> exp
    val load : t -> size -> exp
    val store : t -> size -> exp -> stmt
  end
end

module IA32 : S
module AMD64 : S

