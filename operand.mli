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

val r : op array -> f:(reg -> 'a Or_error.t) -> 'a Or_error.t
val i : op array -> f:(imm -> 'a Or_error.t) -> 'a Or_error.t
val m : op array -> f:(mem -> 'a Or_error.t) -> 'a Or_error.t

val rr : op array -> f:(reg -> reg -> 'a Or_error.t) -> 'a Or_error.t
val ri : op array -> f:(reg -> imm -> 'a Or_error.t ) -> 'a Or_error.t
val rm : op array -> f:(reg -> mem -> 'a Or_error.t) -> 'a Or_error.t
val mr : op array -> f:(mem -> reg -> 'a Or_error.t) -> 'a Or_error.t
val mi : op array -> f:(mem -> imm -> 'a Or_error.t) -> 'a Or_error.t
