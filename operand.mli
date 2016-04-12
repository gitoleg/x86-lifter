open Core_kernel.Std
open Bap.Std

type nonrec reg = reg

type nonrec imm = imm

type mem = {
  seg : reg;
  base : reg;
  scale : imm;
  index : reg;
  disp : imm
}

module Decoder : sig
  type 'a t
  val r : reg t
  val i : imm t
  val m : mem t
  val unary : 'a t -> op array -> 'a option
  val binary : 'a t -> 'b t -> op array -> ('a * 'b) option
  val ternary : 'a t -> 'b t -> 'c t -> op array -> ('a * 'b * 'c) option
end

val r : op array -> reg option
val i : op array -> imm option
val m : op array -> mem option

val rr : op array -> (reg * reg) option
val ri : op array ->  (reg * imm) option
val rm : op array ->  (reg * mem) option
val mr : op array ->  (mem * reg) option
val mi : op array ->  (mem * imm) option

val rr_exn : op array -> reg * reg
val ri_exn : op array -> reg * imm
val rm_exn : op array -> reg * mem
val mr_exn : op array -> mem * reg
val mi_exn : op array -> mem * imm


