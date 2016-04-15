open Core_kernel.Std
open Bap.Std

(** Register representation *)
module type RR = sig
  type t [@@deriving sexp]
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
module type IM = sig
  type t
  val of_imm : Operand.imm -> t
  val get : width:size -> t -> exp
end

(** Memory model *)
module type MM = sig
  type t
  val of_mem : Operand.mem -> t
  val of_offset : mem -> Operand.imm -> t
  val addr : t -> exp
  val load : t -> size:size -> exp
  val store : t -> size:size -> exp -> stmt
end

module type S = sig
  module RR : RR
  module IM : IM
  module MM : MM
end

module type X86CPU = sig
 type regs = private [< Asm.reg]
 val arch : Arch.x86
 val avaliable : Asm.reg -> bool
 include module type of X86_cpu.AMD64
end
