open Core_kernel.Std
open Bap.Std
open Movx_opcode
open Asm.Reg
module Dis = Disasm_expert.Basic

exception Invalid_signature
exception Invalid_operands
module type S = module type of Core_kernel.Std

module Make (CPU : CPU) (Env : X86env.S) (Lifters : X86backend.S) =
struct
  open Env

  let movx_rr (op:movx_rr) ops =
    Operand.rr ops ~f:(fun dst src ->
        let dst = RR.of_mc_exn src in
        let src = RR.of_mc_exn src in
        Ok [RR.get src |> RR.set dst])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_ri (op:movx_ri) ops =
    Operand.ri ops ~f:(fun reg imm ->
        RR.of_mc_exn reg |> fun reg ->
        IM.of_imm imm |> fun imm ->
        Ok [IM.get ~width:(RR.width reg) imm |> RR.set reg])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_mi (op:movx_mi) ops =
    Operand.mi ops ~f:(fun mem imm ->
        let mem = MM.of_mem mem in
        let imm = IM.of_imm imm in
        let size = match op with
          | `MOV8mi -> `r8
          | `MOV16mi -> `r16
          | `MOV32mi -> `r32
          | `MOV64mi32 -> `r64 in
        Ok [IM.get ~width:size imm |>
            MM.store mem ~size])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_rm (op:movx_rm) ops =
    Operand.rm ops ~f:(fun reg mem ->
        let reg = RR.of_mc_exn reg in
        let mem = MM.of_mem mem in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_mr (op:movx_mr) ops =
    Operand.mr ops ~f:(fun mem reg ->
        let mem = MM.of_mem mem in
        let reg = RR.of_mc_exn reg in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_oa (op:movx_oa) ops =
    Operand.i ops ~f:(fun off ->
        let mem = MM.of_offset off in
        let reg =
          let asm = match op with
            | `MOV8o8a
            | `MOV64o8a -> `AL
            | `MOV16o16a
            | `MOV64o16a -> `AX
            | `MOV32o32a
            | `MOV64o32a -> `EAX
            | `MOV64o64a -> `RAX in
          RR.of_asm_exn asm in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx_ao (op:movx_ao) ops =
    Operand.i ops ~f:(fun off ->
        let mem = MM.of_offset off in
        let reg =
          let asm = match op with
            | `MOV8ao8
            | `MOV64ao8 -> `AL
            | `MOV16ao16
            | `MOV64ao16 -> `AX
            | `MOV32ao32
            | `MOV64ao32 -> `EAX
            | `MOV64ao64 -> `RAX in
          RR.of_asm_exn asm in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])
      ~on_error:(Or_error.error_string "invalid operands")

  let movx (op:Movx_opcode.t) ops =
    Or_error.try_with_join (fun () ->
        match op with
        | #movx_rr as op -> movx_rr op ops
        | #movx_ri as op -> movx_ri op ops
        | #movx_rm as op -> movx_rm op ops
        | #movx_mr as op -> movx_mr op ops
        | #movx_mi as op -> movx_mi op ops
        | #movx_oa as op -> movx_oa op ops
        | #movx_ao as op -> movx_ao op ops)

  let register () =
    List.iter ~f:(fun op -> Lifters.register (op :> Opcode.t) (movx op))
      Opcode.all_of_movx
end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64) (X86backend.AMD64)

let register () =
  IA32.register ();
  AMD64.register ()

      
