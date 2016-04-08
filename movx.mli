open Core_kernel.Std

exception Invalid_signature
exception Invalid_operands

val register : unit -> unit list Or_error.t
