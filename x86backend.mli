open Core_kernel.Std
open Bap.Std

include module type of X86backend_types

module IA32 : S
module AMD64 : S
