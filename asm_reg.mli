open Core_kernel.Std
open Bap.Std

include module type of Asm_reg_types

val width : t -> size
val bitwidth : t -> int
val decode : reg -> t option