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


module Reg (CPU : CPU) (Env : Env)= struct
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
    | _ -> mov_errorf "MOVx.movx_rm %s %%%s:%d(%%%s, %%%s, %d), %%%s"
             (sexp_of_movx_rm op |> Sexp.to_string)
             (sexp_of_x86reg seg |> Sexp.to_string)
             disp
             (sexp_of_x86reg base |> Sexp.to_string)
             (sexp_of_x86reg index |> Sexp.to_string)
             scale
             (sexp_of_x86reg dst |> Sexp.to_string)

  let movx_mr op seg base scale index disp src =
    match op, src with
    | `MOV8mr, #r8
    | `MOV16mr, #r16
    | `MOV32mr, #r32
    | `MOV64mr, #r64 ->
      let data = Env.get src in
      let size = Env.width src in
      Env.store ~seg ~base ~scale ~index ~disp size data
    | _ -> mov_errorf "MOVx.movx_mr %s %%%s, %%%s:%d(%%%s, %%%s, %d)"
             (sexp_of_x86reg src |> Sexp.to_string)
             (sexp_of_movx_mr op |> Sexp.to_string)
             (sexp_of_x86reg seg |> Sexp.to_string)
             disp
             (sexp_of_x86reg base |> Sexp.to_string)
             (sexp_of_x86reg index |> Sexp.to_string)
             scale

  let print_insn op ops =
    Printf.printf "%s " 
      (sexp_of_t (op :> t) |> Sexp.to_string);
    Array.iter ~f:(function
        | Op.Reg r -> Printf.printf "%s " (Reg.name r)
        | Op.Imm imm -> Printf.printf "imm:%Lx " (Imm.to_int64 imm)
        | Op.Fmm fmm -> Printf.printf "fmm:%g " (Fmm.to_float fmm))
      ops;
    print_endline ""

  let lift op ops =
    print_insn op ops;
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

    | _ -> mov_errorf
             "Movx.lift unknown %s instruction with operands %s"
             (sexp_of_movx op |> Sexp.to_string)
             (Array.sexp_of_t Op.sexp_of_t ops |> Sexp.to_string)
        
end

module type S = module type of Imm
