open Core_kernel.Std
open Bap.Std

include module type of Asm_reg_types

val width : t -> [`r8 | `r16 | `r32 | `r64] Size.p
val bitwidth : t -> int
val decode : reg -> t option
