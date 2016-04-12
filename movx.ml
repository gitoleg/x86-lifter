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

  let movx_rr op ops =
    let dst, src =  Operand.rr_exn ops |>
                    Tuple.T2.map_fst ~f:RR.of_reg_exn |>
                    Tuple.T2.map_snd ~f:RR.of_reg_exn in
    match RR.width dst = RR.width src with
    | true -> [RR.get src |> RR.set dst]
    | false -> raise Invalid_operands

  let movx_ri op ops =
    let reg, imm = Operand.ri_exn ops |>
                   Tuple.T2.map_fst ~f:RR.of_reg_exn |>
                   Tuple.T2.map_snd ~f:IM.of_imm in
    let value =
      match op, RR.to_x86reg reg with
      | `MOV64ri32, #r64 ->  IM.get ~width:`r32 imm |>
                             Bil.(cast signed 64)
      | `MOV8ri, #r8
      | `MOV16ri, #r16
      | `MOV32ri, #r32
      | `MOV64ri, #r64 -> IM.get ~width:(RR.width reg) imm
      | _ -> raise Invalid_operands in
    [RR.set reg value]

  let movx_mi op ops =
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
      | `MOV32mi -> imm `r32
      | _ -> raise Invalid_operands in
    [MM.store mem (size :> size) data]

  let movx_rm op ops =
    let reg, mem = Operand.rm_exn ops |>
                   Tuple.T2.map_fst ~f:RR.of_reg_exn |>
                   Tuple.T2.map_snd ~f:MM.of_mem in
    match op, RR.to_x86reg reg with
    | `MOV8rm, #r8
    | `MOV16rm, #r16
    | `MOV32rm, #r32
    | `MOV64rm, #r64 ->
      let size = (RR.width reg :> size) in
      [MM.load mem size |>
       RR.set reg]
    | _ -> raise Invalid_operands

  let movx_mr op ops =
    let mem, reg = Operand.mr_exn ops |>
                   Tuple.T2.map_fst ~f:MM.of_mem |>
                   Tuple.T2.map_snd ~f:RR.of_reg_exn in
    match op, RR.to_x86reg reg with
    | `MOV8mr, #r8
    | `MOV16mr, #r16
    | `MOV32mr, #r32
    | `MOV64mr, #r64 ->
      [RR.get reg |>
       MM.store mem (RR.width reg :> size)]
    | _ -> raise Invalid_operands

  let register () =
    let reg t = Backend.register_all (t :> Opcode.t list) in
    [ reg all_of_movx_rr movx_rr;
      reg all_of_movx_ri movx_ri;
      reg all_of_movx_rm movx_rm;
      reg all_of_movx_mr movx_mr;
      reg all_of_movx_mi movx_mi]
end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64) (X86backend.AMD64)

let register () =
  Or_error.combine_errors_unit
    (IA32.register () @
     AMD64.register ())
      
