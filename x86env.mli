open Core_kernel.Std
open Bap.Std
open X86types

exception Unreachable_reg of (Arch.x86 * x86reg)

val env_of_arch : Arch.x86 -> (module Env)

