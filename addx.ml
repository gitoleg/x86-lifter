open Core_kernel.Std
open Bap.Std
open Types

type add_rr = [
  | `ADD64rr
  | `ADD32rr
  | `ADD16rr
  | `ADD8rr
] with sexp

type add_rm = [
  | `ADD64rm
  | `ADD32rm
  | `ADD16rm
  | `ADD8rm
] with sexp

(** TODO: thus looks like subset of different add*ri where reg = *ax  *)
type add_rax = [
  | `ADD64i32
  | `ADD32i32
  | `ADD16i16
  | `ADD8i8
] with sexp

type add_ri8 = [
  | `ADD64ri8
  | `ADD32ri8
  | `ADD16ri8
] with sexp

type add_ri = [
  | `ADD32ri
  | `ADD16ri
  | `ADD8ri
] with sexp

type add_reg = [ add_rr | add_rm | add_rax | add_ri8 | add_ri ]

type add_mr = [
  | `ADD64mr
  | `ADD32mr
  | `ADD16mr
  | `ADD8mr
] with sexp

type add_mi8 = [
  | `ADD64mi8
  | `ADD32mi8
  | `ADD16mi8
] with sexp

type add_mi = [
  | `ADD32mi
  | `ADD16mi
  | `ADD8mi 
] with sexp

type add_mem = [ add_mr | add_mi8 | add_mi ]

type add64_i32 = [
  | `ADD64mi32
  | `ADD64ri32
] with sexp

type t = [ add_reg | add_mem | add64_i32 ]

module Reg(Target:Target) = struct
  module CPU = Target.CPU
                 
  let find_gpr name = Var.Set.find_exn CPU.gpr ~f:(fun v -> Var.name v = name)
  let rax = find_gpr "RAX"
  let eax = find_gpr "EAX"

  let msb r = Bil.(cast high 1 r)
  
  let set_vf res op op' =       
    Bil.(CPU.vf := msb (lnot (op lxor op')) land (op lxor res))

  let set_cf res op = Bil.(CPU.cf := res < op)
  let set_nf r = Bil.(CPU.nf := msb r)

  let set_zf width res =
    Bil.(CPU.zf := res = int (Word.zero width))
  
  let set_flags width res op op' = 
    [set_vf res op op'; 
     set_nf res; 
     set_zf width res; 
     set_cf res op;]

  let exp_of_reg width reg = match width with 
    | 64 -> Env.reg_from_dis64 reg |> Env.reg64
    | 32 -> Env.reg_from_dis32 reg |> Env.reg32
    | _ -> invalid_arg "Add.exp_of_reg: expect (32 | 64)" 

  let var_of_reg width reg = match width with 
    | 64 -> Env.reg_from_dis64 reg |> Env.real64
    | 32 -> Env.reg_from_dis32 reg |> Env.real32
    | _ -> invalid_arg "Add.var_of_reg: expect (32 | 64)" 

  let imm_to_word width v = 
    match Imm.to_word v ~width with
    | Some w -> w
    | None -> 
      Printf.sprintf "Add.imm_to_word: expect %d" width |>
      invalid_arg 

  let add_exp width dst e e' : bil =
    let open Bil.Infix in
    let r = e + e' in
    (dst := r) :: set_flags width r e e'

  let add_rr width op op' : bil = 
    let dst, op = var_of_reg width op, exp_of_reg width op in
    let op' = exp_of_reg width op' in
    add_exp width dst op op'

  let add_ri width dst v : bil = 
    let v = imm_to_word width v in
    let v' = Bil.(cast signed width (int v)) in
    add_exp width (var_of_reg width dst) (exp_of_reg width dst) v'

  let lift op ops = 
    let open Op in
    match op, ops with
    | `ADD64rr, [|Reg dst; Reg r|] -> add_rr 64 dst r
    | `ADD32rr, [|Reg dst; Reg r|] -> add_rr 32 dst r
    | `ADD16rr, [|Reg dst; Reg r|] -> add_rr 16 dst r
    | `ADD8rr,  [|Reg dst; Reg r|] -> add_rr 8  dst r
    | `ADD32ri, [|Reg dst; Imm v|] -> add_ri 32 dst v
    | `ADD16ri, [|Reg dst; Imm v|] -> add_ri 16 dst v
    | `ADD8ri,  [|Reg dst; Imm v|] -> add_ri 8  dst v
    | op,ops -> invalid_arg "invalid operation signature"
end
