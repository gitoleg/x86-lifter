open Core_kernel.Std
open Bap.Std

module type S = X86env_types.S

module IA32 : S
module AMD64 : S

