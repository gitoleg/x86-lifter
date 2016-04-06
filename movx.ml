open Core_kernel.Std
open Bap.Std
open X86types
open Opcode
module Dis = Disasm_expert.Basic

exception Invalid_signature
exception Invalid_operands

module Reg (CPU : CPU) (Env : Env)= struct
  let movx_rr op dst src =
    match Env.width dst = Env.width src with
    | true -> Env.get src |> Env.set dst
    | false -> raise Invalid_operands

  let word_of_imm ~width imm =
    Imm.to_word imm ~width |> Option.value_exn

  let movx_ri op dst src =
    let value =
      match op, dst with
      | `MOV64ri32, #r64 -> word_of_imm ~width:32 src |>
                            Bil.int |>
                            Bil.(cast signed 64)
      | `MOV8ri, #r8
      | `MOV16ri, #r16
      | `MOV32ri, #r32
      | `MOV64ri, #r64 -> word_of_imm ~width:(Env.bitwidth dst) src |>
                          Bil.int
      | _ -> raise Invalid_operands in
    Env.set dst value

  let movx_mi op seg base scale index disp src =
    let imm size =
      word_of_imm ~width:size src |> Bil.int,
      Size.of_int_exn size in
    let data, size =
      match op with
      | `MOV64mi32 -> word_of_imm ~width:32 src |>
                      Bil.int |>
                      Bil.(cast signed 64), `r64
      | `MOV8mi -> imm 8
      | `MOV16mi -> imm 16
      | `MOV32mi -> imm 32 in
    Env.store ~seg ~base ~scale ~index ~disp size data

  let movx_rm op dst seg base scale index disp =
    match op, dst with
    | `MOV8rm, #r8
    | `MOV16rm, #r16
    | `MOV32rm, #r32
    | `MOV64rm, #r64 ->
      let size = Env.width dst in
      Env.load ~seg ~base ~scale ~index ~disp size |>
      Env.set dst
    | _ -> raise Invalid_operands

  let movx_mr op seg base scale index disp src =
    match op, src with
    | `MOV8mr, #r8
    | `MOV16mr, #r16
    | `MOV32mr, #r32
    | `MOV64mr, #r64 ->
      let data = Env.get src in
      let size = Env.width src in
      Env.store ~seg ~base ~scale ~index ~disp size data
    | _ -> raise Invalid_operands

  let lift op ops =
    match op,ops with
    | #movx_rr as op, [|Op.Reg dst; Op.Reg src|] ->
      [ movx_rr op (Env.of_reg dst) (Env.of_reg src) ]
    | #movx_ri as op, [|Op.Reg dst; Op.Imm src|] ->
      [ movx_ri op (Env.of_reg dst) src ]
    | #movx_rm as op, [| Op.Reg dst;
                         Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg |] ->
      [ movx_rm op (Env.of_reg dst)
          (Env.of_reg seg)
          (Env.of_reg base)
          (scale |> Imm.to_int |> Option.value_exn)
          (Env.of_reg index)
          (disp |> Imm.to_int |> Option.value_exn)]
    | #movx_mr as op, [| Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg;
                         Op.Reg src |] ->
      [ movx_mr op (Env.of_reg seg)
          (Env.of_reg base)
          (scale |> Imm.to_int |> Option.value_exn)
          (Env.of_reg index)
          (disp |> Imm.to_int |> Option.value_exn)
          (Env.of_reg src) ]
    | #movx_mi as op, [| Op.Reg base;
                         Op.Imm scale;
                         Op.Reg index;
                         Op.Imm disp;
                         Op.Reg seg;
                         Op.Imm src |] ->
      [ movx_mi op (Env.of_reg seg)
          (Env.of_reg base)
          (scale |> Imm.to_int |> Option.value_exn)
          (Env.of_reg index)
          (disp |> Imm.to_int |> Option.value_exn)
          src ]

    | _ -> raise Invalid_signature
end

module type S = module type of Imm
