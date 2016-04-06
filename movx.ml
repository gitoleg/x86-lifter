open Core_kernel.Std
open Bap.Std
open X86types
open Opcode
module Dis = Disasm_expert.Basic

exception Invalid_signature
exception Invalid_operands

module Reg (CPU : CPU) (Env : Env)= struct
  open Env
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
      | `MOV32mi -> imm `r32 in
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

  let lift op ops =
    match op,ops with
    | #movx_rr as op, [|Op.Reg dst; Op.Reg src|] ->
      [ movx_rr op (RR.of_reg dst) (RR.of_reg src) ]
    | #movx_ri as op, [|Op.Reg dst; Op.Imm src|] ->
      [ movx_ri op (RR.of_reg dst) src ]
    | #movx_rm as op, [| Op.Reg dst;
                         Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg |] ->
      [ movx_rm op (RR.of_reg dst)
          (MM.from_addr ~seg ~base ~scale ~index ~disp) ]
    | #movx_mr as op, [| Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg;
                         Op.Reg src |] ->
      [ movx_mr op (MM.from_addr ~seg ~base ~scale ~index ~disp)
          (RR.of_reg src) ]
    | #movx_mi as op, [| Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg;
                         Op.Imm src |] ->
      [ movx_mi op (MM.from_addr ~seg ~base ~scale ~index ~disp)
          src ]

    | _ -> raise Invalid_signature
end
