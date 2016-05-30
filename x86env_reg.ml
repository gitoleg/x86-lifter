open Core_kernel.Std
open Bap.Std
open X86env_types

module Make(CPU : X86CPU) : RR = struct
  type t = Asm.reg [@@deriving sexp]

  let of_asm = function
    | r when CPU.avaliable r -> Some r
    | r -> None

  let of_mc reg =
    let open Option in
    Asm.Reg.decode reg >>= (function
    | #Asm.reg as t -> of_asm t
    | _ -> None)

  let of_asm_exn reg = of_asm reg |> Option.value_exn

  let of_mc_exn reg = of_mc reg |> Option.value_exn

  let to_asm t = t

  let size = match CPU.arch with `x86 -> `r32 | `x86_64 -> `r64

  let bitsize = size |> Size.in_bits

  let width t = match t with
    | #Asm.Reg.gpr as r -> Asm.Reg.width r
    | #Asm.Reg.segment_base -> size

  let bitwidth t = width t |> Size.in_bits

  let var_all = function
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
    | `FS_BASE -> CPU.fs_base
    | `GS_BASE -> CPU.gs_base

  let var t =
    if CPU.avaliable t then var_all t
    else Error.failwiths "invalid reg variable"
        t Asm.sexp_of_reg

  let bvar r = var r |> Bil.var

  let get r =
    let open Asm in
    let v = bvar r in
    match r, CPU.arch with
    | #Reg.r8h, _ -> Bil.(extract ~hi:15 ~lo:8 v)
    | #Reg.r8l, _ | #Reg.r16, _ | #Reg.r32, `x86_64 ->
      Bil.(cast low (bitwidth r) v)
    | #Reg.r32, `x86 | #Reg.r64, _ | #Reg.segment_base, _ -> v

  let set r e =
    let open Asm in
    let lhs = var r in
    let rhs =
      let v = bvar r in
      match r, CPU.arch with
      | #Reg.r8h, _ ->
        let hp = bitsize - 16 in
        Bil.((cast high hp v) ^ e ^ (cast low 8 v))
      | #Reg.r8l, _ | #Reg.r16, _ ->
        let hp = bitsize - bitwidth r in
        Bil.((cast high hp v) ^ e)
      | #Reg.r32, `x86_64 -> Bil.(cast unsigned bitsize e)
      | #Reg.r32, `x86 | #Reg.r64, `x86_64 | #Reg.segment_base, _ -> e
      | #Reg.r64, `x86 -> Error.failwiths "invalid reg"
                            r Asm.sexp_of_reg in
    Bil.(lhs := rhs)
end
