open Core_kernel.Std
open Bap.Std
open X86types
open Opcode
module Dis = Disasm_expert.Basic

exception Invalid_signature
exception Invalid_operands

module Make (CPU : CPU) (Env : Env)= struct
  open Env
  module Ext = Extract.Make(Env)

  let movx_rr op reg src =
    match RR.width reg = RR.width src with
    | true -> RR.get src |> RR.set reg
    | false -> raise Invalid_operands

  let word_of_imm ~width imm =
    Imm.to_word imm ~width |> Option.value_exn

  let movx_ri op reg imm =
    let value =
      match op, reg with
      | `MOV64ri32, #r64 -> word_of_imm ~width:32 imm |>
                            Bil.int |>
                            Bil.(cast signed 64)
      | `MOV8ri, #r8
      | `MOV16ri, #r16
      | `MOV32ri, #r32
      | `MOV64ri, #r64 -> word_of_imm ~width:(RR.bitwidth reg) imm |>
                          Bil.int
      | _ -> raise Invalid_operands in
    RR.set reg value

  let movx_mi op mem imm =
    let imm size =
      word_of_imm ~width:(Size.in_bits size) imm |> Bil.int, size in
    let data, size =
      match op with
      | `MOV64mi32 ->
        let (d, _) = imm `r32 in
        Bil.(cast signed 64 d), `r64
      | `MOV8mi -> imm `r8
      | `MOV16mi -> imm `r16
      | `MOV32mi -> imm `r32
      | _ -> raise Invalid_operands in
    MM.store mem size data

  let movx_rm op reg mem =
    match op, reg with
    | `MOV8rm, #r8
    | `MOV16rm, #r16
    | `MOV32rm, #r32
    | `MOV64rm, #r64 ->
      let size = RR.width reg in
      MM.load mem size |>
      RR.set reg
    | _ -> raise Invalid_operands

  let movx_mr op mem reg =
    match op, reg with
    | `MOV8mr, #r8
    | `MOV16mr, #r16
    | `MOV32mr, #r32
    | `MOV64mr, #r64 ->
      let data = RR.get reg in
      let size = RR.width reg in
      MM.store mem size data
    | _ -> raise Invalid_operands

  let lift movx extract op ops =
    match extract ops with
    | Some (op1, op2) -> [movx op op1 op2]
    | None -> raise Invalid_operands

  let register register_all =
    let register_all t = register_all (t :> Opcode.t list) in
    [
      register_all Opcode.all_of_movx_rr (lift movx_rr Ext.brr);
      register_all Opcode.all_of_movx_ri (lift movx_ri Ext.bri);
      register_all Opcode.all_of_movx_rm (lift movx_rm Ext.brm);
      register_all Opcode.all_of_movx_mr (lift movx_mr Ext.bmr);
      register_all Opcode.all_of_movx_mi (lift movx_mi Ext.bmi)
    ]
end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64)

let register () =
    IA32.register X86backend.IA32.register_all @
    AMD64.register X86backend.AMD64.register_all |>
    Or_error.combine_errors




