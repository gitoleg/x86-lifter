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
  val of_mem : mem -> Operand.mem -> t
  val of_offset : mem -> Operand.imm -> t
  val addr : t -> exp
  val addr_size : addr_size
  val load_from : exp -> size:size -> exp
  val store_to : exp -> size:size -> exp -> stmt
  val load : t -> size:size -> exp
  val store : t -> size:size -> exp -> stmt
end

(** Flags representation *)
module type FR = sig
  type t = [
    | `CF
    | `PF
    | `AF
    | `ZF
    | `SF
    | `DF
    | `OF
  ]
  val var : t -> var
  val get : t -> exp
  val set : t -> exp -> stmt
  val set_unknown : t -> string -> stmt
  val after_sub : diff:exp -> size -> op1:exp -> op2:exp -> stmt list
  val after_add : sum:exp -> size ->  op1:exp -> op2:exp -> stmt list
end

module type S = sig
  module RR : RR
  module FR : FR
  module IM : IM
  module MM : MM
end

module type X86CPU = sig
 type regs = private [< Asm.reg]
 val arch : Arch.x86
 val avaliable : Asm.reg -> bool
 val cf : var
 val pf : var
 val af : var
 val zf : var
 val sf : var
 val oF : var
 val df : var
 include X86_env.ModeVars
end
