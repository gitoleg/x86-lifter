open Core_kernel.Std
open Bap.Std
open Types
open Addx_opcode
open Adcx_opcode
open Subx_opcode
open Sbbx_opcode

let exp_of_reg width reg = match width with
  | 64 -> X86env.reg_from_dis64 reg |> X86env.reg64
  | 32 -> X86env.reg_from_dis32 reg |> X86env.reg32
  | _ -> invalid_arg "Add.exp_of_reg: expect (32 | 64)"

let var_of_reg width reg = match width with
  | 64 -> X86env.reg_from_dis64 reg |> X86env.real64
  | 32 -> X86env.reg_from_dis32 reg |> X86env.real32
  | _ -> invalid_arg "Add.var_of_reg: expect (32 | 64)"

let imm_to_exp ~width ~value_width value = 
  match Imm.to_word value ~width:value_width with
  | None -> 
    Printf.sprintf "Addx.imm_to_exp: imm expected of %d width" value_width |>
    invalid_arg
  | Some w -> 
    if width > value_width then Bil.(cast signed width (int w))
    else Bil.int w

let prepare_imm ~width ~value_width value =
  let value_width = 
    match value_width with
    | None -> width
    | Some w -> w in
  let data = imm_to_exp ~width ~value_width value in
  let v0 = Var.create ~is_virtual:true "v0" (Type.Imm width) in
  Bil.move v0 data, Bil.var v0 

(** [find_number_exn ~start str] - returns a number from string [str], 
    starting at position [start]. Raise [Not_found] if no numbers found *)
let find_number_exn ?(start=0) str =
  let r = Str.regexp "[0-9]+[0-9]*" in
  let _ = Str.search_forward r str start in
  int_of_string (Str.matched_string str), Str.match_end ()

exception Invalid_arg_set 

module type Arithmetic = sig
  type opcode
  val string_of_opcode: opcode -> string

  (** [process width dst operand operand'] *)
  val process: int -> var -> exp -> exp -> bil
end

module Add(Target:Target) = struct

  type opcode = addx

  module CPU = Target.CPU

  let msb r = Bil.(cast high 1 r)
  let set_cf res op = Bil.(CPU.cf := res < op)
  let set_nf r = Bil.(CPU.nf := msb r)

  let set_vf res op op' =
    Bil.(CPU.vf := msb (lnot (op lxor op')) land (op lxor res))

  let set_zf width res =
    Bil.(CPU.zf := res = int (Word.zero width))
  
  let set_flags width res op op' =
    [set_vf res op op';
     set_nf res;
     set_zf width res;
     set_cf res op;]
    
  let string_of_opcode t = 
    Sexp.to_string (Addx_opcode.sexp_of_addx t)

  let process width dst e e' =
    let open Bil in
    let r = e + e' in
    let flags = set_flags width r e e' in
    List.rev ((dst := r) :: flags)
end

module AddCarry(Target:Target) = struct
  module Add = Add(Target)

  type opcode = adcx
    
  let string_of_opcode t = 
    Sexp.to_string (Adcx_opcode.sexp_of_adcx t)

  let process width dst e e' = 
    let ec = Bil.(e' + Bil.var Target.CPU.cf) in
    Add.process width dst e ec
end

(** TODO: add flags here  *)
module Sub(Target : Target) = struct
  type opcode = subx
    
  let string_of_opcode t = 
    Sexp.to_string (Subx_opcode.sexp_of_subx t)

  let process width dst e e' = 
    let open Bil in
    let r = e - e' in
    (dst := r) :: []
end

module SubBorrow(Target : Target) = struct
  module Sub = Sub(Target)
  type opcode = sbbx
    
  let string_of_opcode t = 
    Sexp.to_string (Sbbx_opcode.sexp_of_sbbx t)

  let process width dst e e' = 
    let eb = Bil.(e' - Bil.var Target.CPU.cf) in
    Sub.process width dst e eb

end

module Make(Target : Target) (Arith : Arithmetic) = struct
  module CPU = Target.CPU
  include Arith

  let mem_var = Target.CPU.mem
  let mem = Bil.var mem_var
  let endian = LittleEndian

  (** [search_number_exn ~index op] - returns a number from [op]
      name, with [index], e.g. `[search_number_exn ~index `ADD64ri32]`
      will return 64 with index = 0 and 32 with index = 1. *)
  let search_number_exn ~index op =   
    let s = string_of_opcode op in
    let rec search cnt start =
      let n,start' = find_number_exn ~start s in
      if cnt = index then n
      else search (cnt + 1) start' in
    search 0 0

  let search_number_opt ~index op = 
    try 
      Some (search_number_exn ~index op)
    with Not_found -> None

  let dst_width_exn op = search_number_exn ~index:0 op
  let src_width_exn op = search_number_exn ~index:1 op
  let src_width_opt op = search_number_opt ~index:1 op

  let load_from_reg width reg =
    let size = Size.of_int_exn width in
    Bil.load ~mem ~addr:(exp_of_reg width reg) endian size 

  let process_rr ~width dst op op' : bil =
    process width 
      (var_of_reg width dst)
      (exp_of_reg width op)
      (exp_of_reg width op') 

  let process_ri ~width ?value_width dst src value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: process width (var_of_reg width dst) (exp_of_reg width src) data

  let process_mi ~width ?value_width dst_reg value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: process width mem_var (exp_of_reg width dst_reg) data

  let process_mr ~width mem_op src : bil =
    let data = load_from_reg width mem_op in
    process width mem_var data (exp_of_reg width src)

  let process_rm ~width dst src mem_op : bil =
    let data = load_from_reg width mem_op in
    process width (var_of_reg width dst) (exp_of_reg width src) data

  (** TODO: it's only stub. refine it  *)
  let process_rax ~width ~value_width value =
    let mv, data = 
      prepare_imm ~width ~value_width:(Some value_width) value in
    let dst, src, full_width = match Var.typ CPU.sp with
      | Type.Imm 32 -> X86env.real32 `EAX, X86env.reg32 `EAX, 32
      | Type.Imm 64 -> X86env.real64 `RAX, X86env.reg64 `RAX, 64
      | _ -> failwith "Addx.add_rax: unexpected width" in
    if full_width = width then
      mv :: process width dst src data
    else
      let low = Bil.extract (value_width - 1) 0 src in
      let high = Bil.extract (full_width - 1) value_width src in
      let v1 = Var.create ~is_virtual:true "v1" (Type.Imm value_width) in
      let v1' = Bil.var v1 in
      let s = Bil.move v1 low in
      let add = s :: mv :: process width v1 v1' data in
      let res = Bil.move dst (Bil.concat high v1') :: [] in
      add @ res 
            
  let lift_rr op ops = 
    let open Op in
    match ops with
    | [|Reg dst; Reg src; Reg src'|] -> 
      let width = dst_width_exn op in
      process_rr ~width dst src src'
    | _ -> raise Invalid_arg_set

  let lift_ri op ops = 
    let open Op in
    match ops with
    | [|Reg dst; Reg src; Imm v|] -> 
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      let mv, data = prepare_imm ~width ~value_width v in
      mv :: process width (var_of_reg width dst) (exp_of_reg width src) data
    | _ -> raise Invalid_arg_set

  let lift_rax op ops = 
    let open Op in 
    match ops with
    | [| Imm v |] ->
      let width = dst_width_exn op in
      let width' = src_width_exn op in
      process_rax ~width ~value_width:width' v
    | _ -> raise Invalid_arg_set

  let lift_rm op ops = 
    let open Op in 
    match ops with
    | [|Reg dst; Reg src; Reg base; Imm scale; Reg index; Imm disp; Reg seg|] ->
      let width = dst_width_exn op in
      process_rm ~width dst src base
    | _ -> raise Invalid_arg_set

  let lift_mi op ops = 
    let open Op in
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Imm v |] ->
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      process_mi ~width ?value_width base v
    | _ -> raise Invalid_arg_set
  
  let lift_mr op ops = 
    let open Op in
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Reg src |] ->
      let width = dst_width_exn op in
      process_mr ~width base src
    | _  -> raise Invalid_arg_set

end

(** TODO: it's a kind of trash .. refine it  *)
module Reg(Target:Target) = struct
  module AddPlain = Add(Target)
  module AddCarry = AddCarry(Target)
  module SubPlain = Sub(Target)
  module SubB = SubBorrow(Target)
  module Add = Make(Target)(AddPlain)
  module Adc = Make(Target)(AddCarry)
  module Sub = Make(Target)(SubPlain)
  module Sbb = Make(Target)(SubB)

  let lift op ops =
    try
      match op with
      | (#adc_ri | #adc_ri8 | `ADC64ri32) as op' -> Adc.lift_ri op' ops
      | (#add_ri | #add_ri8 | `ADD64ri32) as op' -> Add.lift_ri op' ops
      | (#sub_ri | #sub_ri8 | `SUB64ri32) as op' -> Sub.lift_ri op' ops
      | (#sbb_ri | #sbb_ri8 | `SBB64ri32) as op' -> Sbb.lift_ri op' ops
      | (#adc_mi | #adc_mi8 | `ADC64mi32) as op' -> Adc.lift_mi op' ops
      | (#add_mi | #add_mi8 | `ADD64mi32) as op' -> Add.lift_mi op' ops
      | (#sub_mi | #sub_mi8 | `SUB64mi32) as op' -> Sub.lift_mi op' ops
      | (#sbb_mi | #sbb_mi8 | `SBB64mi32) as op' -> Sbb.lift_mi op' ops
      | #adc_rr  as op' -> Adc.lift_rr  op' ops
      | #add_rr  as op' -> Add.lift_rr  op' ops
      | #sub_rr  as op' -> Sub.lift_rr  op' ops
      | #sbb_rr  as op' -> Sbb.lift_rr  op' ops
      | #adc_rm  as op' -> Adc.lift_rm  op' ops
      | #add_rm  as op' -> Add.lift_rm  op' ops
      | #sub_rm  as op' -> Sub.lift_rm  op' ops
      | #sbb_rm  as op' -> Sbb.lift_rm  op' ops
      | #adc_mr  as op' -> Adc.lift_mr  op' ops
      | #add_mr  as op' -> Add.lift_mr  op' ops
      | #sub_mr  as op' -> Sub.lift_mr  op' ops
      | #sbb_mr  as op' -> Sbb.lift_mr  op' ops
      | #adc_rax as op' -> Adc.lift_rax op' ops
      | #add_rax as op' -> Add.lift_rax op' ops
      | #sub_rax as op' -> Sub.lift_rax op' ops
      | #sbb_rax as op' -> Sbb.lift_rax op' ops
    with _ -> 
      let s = match op with
        | #adcx as op' -> Adc.string_of_opcode op' 
        | #addx as op' -> Add.string_of_opcode op' 
        | #subx as op' -> Sub.string_of_opcode op' 
        | #sbbx as op' -> Sbb.string_of_opcode op' in
      invalid_arg (Printf.sprintf "invalid operands set for %s" s)
end
