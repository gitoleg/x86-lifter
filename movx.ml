open Core_kernel.Std
open Bap.Std
open X86types
open Opcode
module Dis = Disasm_expert.Basic

exception Mov_error of string

let mov_error str =
  raise (Mov_error str)

let mov_errorf : 'a. ('a, unit, string, 'b) format4 -> 'a = fun fmt ->
  Printf.ksprintf mov_error fmt


module Reg (Env : Env)= struct
  let movx_rr op dst src =
    if Env.width dst = Env.width src then
      Env.get src |> Env.set dst
    else
      mov_errorf "Movx.movx_rr: %s %%%s, %%%s : invalid operands size"
             (sexp_of_movx_rr op |> Sexp.to_string)
             (sexp_of_x86reg src |> Sexp.to_string)
             (sexp_of_x86reg dst |> Sexp.to_string)
        
  let word_of_imm ~width imm =
    match Imm.to_word imm ~width with
    | Some w -> w
    | None ->
      mov_errorf "Movx.word_of_imm : failed get word width %d
                  from immediate operand %s"
        width
        (sexp_of_imm imm |> Sexp.to_string)

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
      | _ -> mov_errorf "Movx.movx_rr %s $%s, %%%s"
             (sexp_of_movx_ri op |> Sexp.to_string)
             (sexp_of_imm src |> Sexp.to_string)
             (sexp_of_x86reg dst |> Sexp.to_string) in
    Env.set dst value

  let lift op ops =
    match op,ops with
    | #movx_rr as op, [|Op.Reg dst; Op.Reg src|] ->
      [ movx_rr op (Env.of_reg dst) (Env.of_reg src) ]
    | #movx_ri as op, [|Op.Reg dst; Op.Imm src|] ->
      [ movx_ri op (Env.of_reg dst) src ]
    | _ -> mov_errorf "Movx.lift unknown %s instruction with operands %s"
                   (sexp_of_movx op |> Sexp.to_string)
                   (Array.sexp_of_t Op.sexp_of_t ops |> Sexp.to_string)
        
end
