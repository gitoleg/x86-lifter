open Core_kernel.Std
open Bap.Std
open Types
open Addx_opcode
open Adcx_opcode
open Subx_opcode
open Sbbx_opcode

(** TODO: rename module or split it somehow  *)
let print msg =
  let () = Printf.printf "%s\n" msg in
  flush stdout

let msb r = Bil.(cast high 1 r)

(** TODO: it's error here, we should use extract  *)
let exp_of_reg width reg = match width with
  | 64 -> X86env.reg_from_dis64 reg |> X86env.reg64
  | 32 -> X86env.reg_from_dis32 reg |> X86env.reg32
  | _ -> invalid_arg "Add.exp_of_reg: expect (32 | 64)"

let var_of_reg width reg = match width with
  | 64 -> X86env.reg_from_dis64 reg |> X86env.real64
  | 32 -> X86env.reg_from_dis32 reg |> X86env.real32
  | _ -> invalid_arg "Add.var_of_reg: expect (32 | 64)"

(** [exp_of_imm ~width ~value_width ~value] - returns 
    an expression, that contains immediate [value]
    and performs sign-extension if neccessary *)
let exp_of_imm ~width ~value_width value = 
  match Imm.to_word value ~width:value_width with
  | None -> 
    Printf.sprintf "Addx.exp_of_imm: imm expected of %d width" value_width |>
    invalid_arg
  | Some w -> 
    if width > value_width then Bil.(cast signed width (int w))
    else Bil.int w

(** [prepare_imm ~width ~value_width value] - creates a 
    temporary variable associated with [value] and returns
    a stmt and exp for it. *)
let prepare_imm ~width ~value_width value =
  let value_width = 
    match value_width with
    | None -> width
    | Some w -> w in
  let data = exp_of_imm ~width ~value_width value in
  let v0 = Var.create ~fresh:true ~is_virtual:true "v0" (Type.Imm width) in
  Bil.move v0 data, Bil.var v0 

let extract ~full_width ~width dst =
  let hd = Bil.extract (full_width - 1) width dst in
  let tl = Bil.extract (width - 1) 0 dst in
  hd, tl

(** [find_number_exn ~start str] - returns a number from string [str], 
    starting at position [start]. Raise [Not_found] if no numbers found *)
let find_number_exn ?(start=0) str =
  let r = Str.regexp "[0-9]+[0-9]*" in
  let _ = Str.search_forward r str start in
  int_of_string (Str.matched_string str), Str.match_end ()

(** module type Arithmetic for defining semantic of instruction *)
module type Arithmetic = sig
  type opcode
  val string_of_opcode: opcode -> string

  (** [process width destination operand operand'] *)
  val process: int -> var -> exp -> exp -> bil
end

module Flags(Target : Target) = struct
  open Target.CPU

  let set_nf res = Bil.(nf := msb res)

  let set_vf res op op' =
    Bil.(vf := msb (lnot (op lxor op')) land (op lxor res))

  let set_zf width res =
    Bil.(zf := res = int (Word.zero width))

  let set_nvzf width res op op' = 
    [set_vf res op op';
     set_nf res;
     set_zf width res;]

end

module Add(Target:Target) = struct
  open Target
  include Flags(Target)

  type opcode = addx

  let set_flags width res op op' =
    Bil.(CPU.cf := res < op) :: set_nvzf width res op op'
    
  let string_of_opcode t = 
    Sexp.to_string (Addx_opcode.sexp_of_addx t)

  let process width dst e e' =   
    print "  label add 0";
    let open Bil in
    let r = e + e' in
    let flags = set_flags width r e e' in
    List.rev ((dst := r) :: flags)
end

(** TODO: think about case when add carry switch on carry *)
module AddCarry(Target:Target) = struct  
  module Add = Add(Target)

  type opcode = adcx
    
  let string_of_opcode t = 
    Sexp.to_string (Adcx_opcode.sexp_of_adcx t)

  let process width dst e e' = 
    let ec = Bil.(e' + cast unsigned width (Bil.var Target.CPU.cf)) in 
    Add.process width dst e ec
end

(** TODO: add flags here  *)
module Sub(Target : Target) = struct
  open Target
  include Flags(Target)

  type opcode = subx
  
  let set_flags width res op op' =
    Bil.(CPU.cf := op < op') :: set_nvzf width res op op'

  let string_of_opcode t = 
    Sexp.to_string (Subx_opcode.sexp_of_subx t)

  let process width dst e e' = 
    print "  label sub 0";
    let open Bil in
    let r = e - e' in
    (dst := r) :: []
end

(** TODO: think about - add or sub carry  *)
module SubBorrow(Target : Target) = struct
  module Sub = Sub(Target)
  type opcode = sbbx
    
  let string_of_opcode t = 
    Sexp.to_string (Sbbx_opcode.sexp_of_sbbx t)

  let process width dst e e' = 
    print "  label sbb 0";
    let eb = Bil.(e' - Bil.var Target.CPU.cf) in
    Sub.process width dst e eb

end

exception Invalid_arg_set 

module Make(Target : Target) (Arith : Arithmetic) = struct
  open Target
  include Arith

  type op = Op.t =
    | Reg of reg
    | Imm of imm
    | Fmm of fmm

  let mem_var = CPU.mem
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
  let dst_width_opt op = search_number_opt ~index:0 op
  let src_width_exn op = search_number_exn ~index:1 op
  let src_width_opt op = search_number_opt ~index:1 op

  let load_from_reg width reg =
    let size = Size.of_int_exn width in
    Bil.load ~mem ~addr:(exp_of_reg width reg) endian size 

  let extract_from_big width exp = 
    let _exp, full_width = match Var.typ CPU.sp with
      | Type.Imm 32 -> X86env.reg32 `EAX, 32
      | Type.Imm 64 -> X86env.reg64 `RAX, 64
      | _ -> failwith "Addx.add_rax: unexpected width" in
    if full_width = width then None
    else Some (extract ~full_width ~width exp)

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
      let high, low = extract ~full_width ~width:value_width src in
      let v1 = Var.create ~fresh:true ~is_virtual:true "v1" (Type.Imm value_width) in
      let v1' = Bil.var v1 in
      let s = Bil.move v1 low in
      let add = s :: mv :: process width v1 v1' data in
      let res = Bil.move dst (Bil.concat high v1') :: [] in
      add @ res 
            
  let lift_rr op ops = 
    match ops with
    | [|Reg dst; Reg src; Reg src'|] -> 
      let width = dst_width_exn op in
      process width 
        (var_of_reg width dst)
        (exp_of_reg width src)
        (exp_of_reg width src') 
    | _ -> raise Invalid_arg_set
     
  let lift_ri op ops = 
    match ops with
    | [|Reg dst; Reg src; Imm v|] -> 
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      let mv, data = prepare_imm ~width ~value_width v in
      mv :: process width (var_of_reg width dst) (exp_of_reg width src) data
    | _ -> 
      let () = Printf.printf "array size %d\n" (Array.length ops) in
      raise Invalid_arg_set

  let lift_rax op ops = 
    match ops with
    | [| Imm v |] ->
      let width = dst_width_exn op in
      let width' = src_width_exn op in
      process_rax ~width ~value_width:width' v
    | _ -> raise Invalid_arg_set

  let lift_rm op ops = 
    match ops with
    | [|Reg dst; Reg src; Reg base; Imm scale; Reg index; Imm disp; Reg seg|] ->
      let width = dst_width_exn op in
      let data = load_from_reg width base in
      process width (var_of_reg width dst) (exp_of_reg width src) data
    | _ -> raise Invalid_arg_set

  let lift_mi op ops = 
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Imm v |] ->
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      let mv, data = prepare_imm ~width ~value_width v in
      mv :: process width mem_var (exp_of_reg width base) data
    | _ -> raise Invalid_arg_set
  
  let lift_mr op ops = 
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Reg src |] ->
      let width = dst_width_exn op in
      let data = load_from_reg width base in
      process width mem_var data (exp_of_reg width src)       
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
      | (#add_ri | #add_ri8 | `ADD64ri32) as op' -> 
        let () = Printf.printf "Add\n" in
        Add.lift_ri op' ops
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
    with Invalid_arg_set -> 
      let s = match op with
        | #adcx as op' -> Adc.string_of_opcode op' 
        | #addx as op' -> Add.string_of_opcode op' 
        | #subx as op' -> Sub.string_of_opcode op' 
        | #sbbx as op' -> Sbb.string_of_opcode op' in
      invalid_arg (Printf.sprintf "invalid operands set for %s" s)
end
