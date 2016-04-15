open Core_kernel.Std
open Bap.Std
open X86env_types

module type S = X86env_types.S

module Make (CPU : X86CPU) : S = struct
  module RR = X86env_reg.Make (CPU)
  module IM = X86env_imm
  module MM = X86env_mem.Make (CPU) (RR) (IM) 
end

module IA32CPU : X86CPU = struct
 type regs = [
   | `AL | `BL | `CL | `DL
   | `AH | `BH | `CH | `DH
   | `AX | `BX | `CX | `DX
   | `DI | `SI | `BP | `SP
   | `EAX | `EBX | `ECX | `EDX
   | `EDI | `ESI | `EBP | `ESP
 ]

 let arch = `x86
 let avaliable = function
   | #regs -> true
   | _ -> false
 include X86_cpu.IA32
end

module AMD64CPU : X86CPU = struct
 type regs = Asm.reg

 let arch = `x86_64
 let avaliable = function
   | #regs -> true
   | _ -> false
 include X86_cpu.AMD64
end

module IA32 = Make(IA32CPU)
module AMD64 = Make(AMD64CPU)
