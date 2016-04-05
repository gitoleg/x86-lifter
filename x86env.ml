open Core_kernel.Std
open Bap.Std
open X86types

exception Unreachable_reg of (Arch.x86 * x86reg)

let unreachable_reg arch x86reg =
  raise (Unreachable_reg (arch, x86reg))

module type X86CPU = sig
 type regs = private [< x86reg]
 val arch : Arch.x86
 val avaliable : x86reg -> bool
 include module type of X86_cpu.AMD64
end

module Make(CPU : X86CPU) : Env = struct
  let of_reg reg =
    Reg.name reg |>
    Sexp.of_string |>
    x86reg_of_sexp |>
    fun r -> match CPU.avaliable r with
    | true -> r
    | false -> unreachable_reg CPU.arch r

  let var = function
    | `AL | `AH | `AX | `EAX | `RAX -> CPU.rax
    | `DL | `DH | `DX | `EDX | `RDX -> CPU.rdx
    | `CL | `CH | `CX | `ECX | `RCX -> CPU.rcx
    | `BL | `BH | `BX | `EBX | `RBX -> CPU.rbx
    | `DIL | `DI | `EDI | `RDI -> CPU.rdi
    | `BPL | `BP | `EBP | `RBP -> CPU.rbp
    | `SPL | `SP | `ESP | `RSP -> CPU.rsp
    | `SIL | `SI | `ESI | `RSI -> CPU.rsi
    | `R8B | `R8W | `R8D | `R8 -> CPU.r.(0)
    | `R9B | `R9W | `R9D | `R9 -> CPU.r.(1)
    | `R10B | `R10W | `R10D | `R10 -> CPU.r.(2)
    | `R11B | `R11W | `R11D | `R11 -> CPU.r.(3)
    | `R12B | `R12W | `R12D | `R12 -> CPU.r.(4)
    | `R13B | `R13W | `R13D | `R13 -> CPU.r.(5)
    | `R14B | `R14W | `R14D | `R14 -> CPU.r.(6)
    | `R15B | `R15W | `R15D | `R15 -> CPU.r.(7)

  let size = match CPU.arch with `x86 -> `r32 | `x86_64 -> `r64

  let width = function
    | #r8 -> `r8
    | #r16 -> `r16
    | #r32 -> `r32
    | #r64 -> `r64

  let bitwidth x86reg = width x86reg |> Size.in_bits

  let bitsize = size |> Size.in_bits

  let bvar r = var r |> Bil.var

  let get r =
    let v = bvar r in
    match r, CPU.arch with
    | #r8h, _ -> Bil.(extract ~hi:8 ~lo:15 v)
    | #r8l, _ | #r16, _ | #r32, `x86_64 ->
      Bil.(cast low (bitwidth r) v)
    | #r32, `x86 | #r64, _ -> v

  let set r e =
    let lhs = var r in
    let rhs =
      let v = bvar r in
      match r, CPU.arch with
      | #r8h, _ ->
        let hp = bitsize - 16 in
        Bil.((cast high hp v) ^ e ^ (cast low 8 v))
      | #r8l, _ | #r16, _ ->
        let hp = bitsize - bitwidth r in
        Bil.((cast high hp v) ^ e)
      | #r32, `x86_64 -> Bil.(cast unsigned bitsize e)
      | #r32, `x86 | #r64, _ -> e in
    Bil.(lhs := rhs)
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
 type regs = x86reg
 let arch = `x86_64
 let avaliable = function
   | #regs -> true
   | _ -> false
 include X86_cpu.AMD64
end

let env_of_arch = function
  | `x86 -> (module Make(IA32CPU) : Env)
  | `x86_64 -> (module Make(AMD64CPU) : Env)
