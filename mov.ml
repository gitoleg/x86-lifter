open Core_kernel.Std
open Bap.Std
open Mov_opcode
open Asm.Reg

module Make (Env : X86env.S) = struct
  open Env

  let mov_rr (op:mov_rr) _mem ops =
    Operand.rr ops ~f:(fun dst src ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        Ok [RR.get src |> RR.set dst])

  let mov_ri (op:mov_ri) _mem ops =
    Operand.ri ops ~f:(fun reg imm ->
        RR.of_mc_exn reg |> fun reg ->
        IM.of_imm imm |> fun imm ->
        Ok [IM.get ~width:(RR.width reg) imm |> RR.set reg])

  let mov_mi (op:mov_mi) mem ops =
    Operand.mi ops ~f:(fun mo imm ->
        let mem = MM.of_mem mem mo in
        let imm = IM.of_imm imm in
        let size = match op with
          | `MOV8mi -> `r8
          | `MOV16mi -> `r16
          | `MOV32mi -> `r32
          | `MOV64mi32 -> `r64 in
        Ok [IM.get ~width:size imm |>
            MM.store mem ~size])

  let mov_rm (op:mov_rm) mem ops =
    Operand.rm ops ~f:(fun reg mo ->
        let reg = RR.of_mc_exn reg in
        let mem = MM.of_mem mem mo in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])

  let mov_mr (op:mov_mr) mem ops =
    Operand.mr ops ~f:(fun mo reg ->
        let mem = MM.of_mem mem mo in
        let reg = RR.of_mc_exn reg in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])

  let mov_oa (op:mov_oa) mem ops =
    Operand.i ops ~f:(fun off ->
        let mem = MM.of_offset mem off in
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

  let mov_ao (op:mov_ao) mem ops =
    Operand.i ops ~f:(fun off ->
        let mem = MM.of_offset mem off in
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

  let mov (op:Mov_opcode.t) =
    match op with
    | #mov_rr as op -> mov_rr op
    | #mov_ri as op -> mov_ri op
    | #mov_rm as op -> mov_rm op
    | #mov_mr as op -> mov_mr op
    | #mov_mi as op -> mov_mi op
    | #mov_oa as op -> mov_oa op
    | #mov_ao as op -> mov_ao op
end

module IA32 = Make (X86env.IA32)
module AMD64 = Make (X86env.AMD64)

let register () =
  List.iter (all_of_mov_ia32 :> Mov_opcode.t list) ~f:(fun op ->
      X86backend.IA32.register (op :> Opcode.t) (IA32.mov op));
  List.iter (all_of_mov_amd64 :> Mov_opcode.t list) ~f:(fun op ->
      X86backend.AMD64.register (op :> Opcode.t) AMD64.(mov op));
