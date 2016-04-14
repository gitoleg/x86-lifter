open Core_kernel.Std
open Bap.Std
open Movx_opcode
open X86reg
module Dis = Disasm_expert.Basic

exception Invalid_signature
exception Invalid_operands
module type S = module type of Core_kernel.Std

module Make (CPU : CPU) (Env : X86env.S) (Backend : X86backend.S) =
struct
  open Env

  let movx_rr (op:movx_rr) ops =
    let dst, src =  Operand.rr_exn ops |>
                    Tuple.T2.map_fst ~f:RR.of_mc_exn |>
                    Tuple.T2.map_snd ~f:RR.of_mc_exn in
    match RR.width dst = RR.width src with
    | true -> [RR.get src |> RR.set dst]
    | false -> raise Invalid_operands

  let movx_ri (op:movx_ri) ops =
    let reg, imm = Operand.ri_exn ops |>
                   Tuple.T2.map_fst ~f:RR.of_mc_exn |>
                   Tuple.T2.map_snd ~f:IM.of_imm in
    let value =
      match op, RR.to_asm reg with
      | `MOV64ri32, #r64 ->  IM.get ~width:`r32 imm |>
                             Bil.(cast signed 64)
      | `MOV8ri, #r8
      | `MOV16ri, #r16
      | `MOV32ri, #r32
      | `MOV64ri, #r64 -> IM.get ~width:(RR.width reg) imm
      | _ -> raise Invalid_operands in
    [RR.set reg value]

  let movx_mi (op:movx_mi) ops =
    let mem, imm = Operand.mi_exn ops |>
                   Tuple.T2.map_fst ~f:MM.of_mem |>
                   Tuple.T2.map_snd ~f:IM.of_imm in
    let imm size = IM.get ~width:size imm |>
                   fun word -> word, size in
    let data, size =
      match op with
      | `MOV64mi32 ->
        let (d, _) = imm `r32 in
        Bil.(cast signed 64 d), `r64
      | `MOV8mi -> imm `r8
      | `MOV16mi -> imm `r16
      | `MOV32mi -> imm `r32 in
    [MM.store mem (size :> size) data]

  let movx_rm (op:movx_rm) ops =
    let reg, mem = Operand.rm_exn ops |>
                   Tuple.T2.map_fst ~f:RR.of_mc_exn |>
                   Tuple.T2.map_snd ~f:MM.of_mem in
    match op, RR.to_asm reg with
    | `MOV8rm, #r8
    | `MOV8rm_NOREX, #r8
    | `MOV16rm, #r16
    | `MOV32rm, #r32
    | `MOV64rm, #r64 ->
      let size = (RR.width reg :> size) in
      [MM.load mem size |>
       RR.set reg]
    | _ -> raise Invalid_operands

  let movx_mr (op:movx_mr) ops =
    let mem, reg = Operand.mr_exn ops |>
                   Tuple.T2.map_fst ~f:MM.of_mem |>
                   Tuple.T2.map_snd ~f:RR.of_mc_exn in
    match op, RR.to_asm reg with
    | `MOV8mr, #r8
    | `MOV8mr_NOREX, #r8
    | `MOV16mr, #r16
    | `MOV32mr, #r32
    | `MOV64mr, #r64 ->
      [RR.get reg |>
       MM.store mem (RR.width reg :> size)]
    | _ -> raise Invalid_operands

  let movx_oa (op:movx_oa) ops =
    let mem = Operand.i_exn ops |> MM.of_offset in
    let reg =
      match op with
      | `MOV8o8a
      | `MOV64o8a -> `AL
      | `MOV16o16a
      | `MOV64o16a -> `AX
      | `MOV32o32a
      | `MOV64o32a -> `EAX
      | `MOV64o64a -> `RAX in
    let reg = RR.of_asm_exn reg in
    [MM.load mem (RR.width reg :> size) |>
     RR.set reg]

  let movx_ao (op:movx_ao) ops =
    let mem = Operand.i_exn ops |> MM.of_offset in
    let reg =
      match op with
      | `MOV8ao8
      | `MOV64ao8 -> `AL
      | `MOV16ao16
      | `MOV64ao16 -> `AX
      | `MOV32ao32
      | `MOV64ao32 -> `EAX
      | `MOV64ao64 -> `RAX in
    let reg = RR.of_asm_exn reg in
    [RR.get reg |>
     MM.store mem (RR.width reg :> size)]

  let movx op ops =
    match op with
    | #movx_rr as op -> movx_rr op ops
    | #movx_ri as op -> movx_ri op ops
    | #movx_rm as op -> movx_rm op ops
    | #movx_mr as op -> movx_mr op ops
    | #movx_mi as op -> movx_mi op ops
    | _ -> Error.failwiths "invalid operation" op Opcode.sexp_of_t

  let register () =
    Backend.register_all (Opcode.all_of_movx :> Opcode.t list) movx
end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64) (X86backend.AMD64)

let register () =
  Or_error.combine_errors_unit [IA32.register (); AMD64.register ()]
      
