open Core_kernel.Std
open Bap.Std

module type S = sig
  (** Register representation *)
  module RR : sig
    type t
    val of_asm : Asm.reg -> t option
    val of_asm_exn : Asm.reg -> t
    val of_mc : Operand.reg -> t option
    val of_mc_exn : Operand.reg -> t
    val to_asm : t -> Asm.reg
    val width : t -> size
    val var : t -> var
    val size : [`r32 | `r64]
    val get : t -> exp
    val set : t -> exp -> stmt
  end

  (**Imm model*)
  module IM : sig
    type t
    val of_imm : Operand.imm -> t
    val get : width:size -> t -> exp
  end

  (** Memory model *)
  module MM : sig
    type t
    val of_mem : Operand.mem -> t
    val of_offset : Operand.imm -> t
    val addr : t -> exp
    val load : t -> size:size -> exp
    val store : t -> size:size -> exp -> stmt
  end
end

module IA32 : S
module AMD64 : S

