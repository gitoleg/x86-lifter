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

val r : op array -> on_error:'a -> f:(reg -> 'a) -> 'a
val i : op array -> on_error:'a -> f:(imm -> 'a) -> 'a
val m : op array -> on_error:'a -> f:(mem -> 'a) -> 'a

val rr : op array -> on_error:'a -> f:(reg -> reg -> 'a) -> 'a
val ri : op array -> on_error:'a -> f:(reg -> imm -> 'a ) -> 'a
val rm : op array -> on_error:'a -> f:(reg -> mem -> 'a) -> 'a
val mr : op array -> on_error:'a -> f:(mem -> reg -> 'a) -> 'a
val mi : op array -> on_error:'a -> f:(mem -> imm -> 'a) -> 'a
