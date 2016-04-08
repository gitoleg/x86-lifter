open Core_kernel.Std
open Bap.Std
open X86types

module type S = sig
  type reg
  type imm
  type mem
  val ur : op array -> reg option
  val ui : op array -> imm option
  val um : op array -> mem option
  val brr : op array -> (reg * reg) option
  val bri : op array -> (reg * imm) option
  val brm : op array -> (reg * mem) option
  val bmr : op array -> (mem * reg) option
  val bmi : op array -> (mem * imm) option
end

module Make (Env : Env) : S
  with type reg = x86reg
  with type imm = imm
  with type mem = Env.MM.t

