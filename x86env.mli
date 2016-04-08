open Core_kernel.Std
open Bap.Std
open X86types

exception Unreachable_reg of (Arch.x86 * x86reg) [@@deriving sexp]
exception Unreachable_var of (Arch.x86 * x86reg) [@@deriving sexp]
exception Invalid_addr of (Arch.x86 * x86reg * x86reg 
                           * int * x86reg * int) [@@deriving sexp]

module IA32 : Env
module AMD64 : Env

