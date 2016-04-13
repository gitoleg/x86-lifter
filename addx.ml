open Core_kernel.Std
open Bap.Std
open Types
open Addx_opcode

exception Invalid_arg_set of addx

module Reg(Target:Target) = struct
  module CPU = Target.CPU

  let mem_var = CPU.mem
  let mem = Bil.var mem_var
  let endian = LittleEndian

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

  let load_from_reg width reg =
    let size = Size.of_int_exn width in
    Bil.load ~mem ~addr:(exp_of_reg width reg) endian size 

  let prepare_imm ~width ~value_width value =
    let value_width = 
      match value_width with
      | None -> width
      | Some w -> w in
    let data = imm_to_exp ~width ~value_width value in
    let v0 = Var.create ~is_virtual:true "v0" (Type.Imm width) in
    Bil.move v0 data, Bil.var v0 

  let add_exp width dst e e' : bil =
    let open Bil.Infix in
    let r = e + e' in
    let flags = set_flags width r e e' in
    List.rev ((dst := r) :: flags)

  let add_rr ~width dst op op' : bil =
    add_exp width 
      (var_of_reg width dst)
      (exp_of_reg width op)
      (exp_of_reg width op') 

  let add_ri ~width ?value_width dst src value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: add_exp width (var_of_reg width dst) (exp_of_reg width src) data

  let add_mi ~width ?value_width dst_reg value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: add_exp width mem_var (exp_of_reg width dst_reg) data

  let add_mr ~width mem_op src : bil =
    let data = load_from_reg width mem_op in
    add_exp width mem_var data (exp_of_reg width src)

  let add_rm ~width dst src mem_op : bil =
    let data = load_from_reg width mem_op in
    add_exp width (var_of_reg width dst) (exp_of_reg width src) data

  let add_rax ~width ~value_width value =
    let mv, data = 
      prepare_imm ~width ~value_width:(Some value_width) value in
    let dst, src, full_width = match Var.typ CPU.sp with
      | Type.Imm 32 -> X86env.real32 `EAX, X86env.reg32 `EAX, 32
      | Type.Imm 64 -> X86env.real64 `RAX, X86env.reg64 `RAX, 64
      | _ -> failwith "Addx.add_rax: unexpected width" in
    if full_width = width then
      mv :: add_exp width dst src data
    else
      let low = Bil.extract (value_width - 1) 0 src in
      let high = Bil.extract (full_width - 1) value_width src in
      let v1 = Var.create ~is_virtual:true "v1" (Type.Imm value_width) in
      let v1' = Bil.var v1 in
      let s = Bil.move v1 low in
      let add = s :: mv :: add_exp width v1 v1' data in
      let res = Bil.move dst (Bil.concat high v1') :: [] in
      add @ res 
    
  (** [find_number_exn ~start str] - returns a number from string [str], 
      starting at position [start]. Raise [Not_found] if no numbers found *)
  let find_number_exn ?(start=0) str =
    let r = Str.regexp "[0-9]+[0-9]*" in
    let _ = Str.search_forward r str start in
    int_of_string (Str.matched_string str), Str.match_end ()

  (** [search_number_exn ~index op] - returns a number from [op]
      name, with [index], e.g. `[search_number_exn ~index `ADD64ri32]`
      will return 64 with index = 0 and 32 with index = 1. *)
  let search_number_exn ~index op =   
    let s = Sexp.to_string (Addx_opcode.sexp_of_addx op) in
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
         
  let lift_rr op ops = 
    let open Op in
    match ops with
    | [|Reg dst; Reg src; Reg src'|] -> 
      let width = dst_width_exn op in
      add_rr ~width dst src src'
    | _ -> raise (Invalid_arg_set op)

  let lift_ri op ops = 
    let open Op in
    match ops with
    | [|Reg dst; Reg src; Imm v|] -> 
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      add_ri ~width ?value_width dst src v
    | _ -> raise (Invalid_arg_set op)

  let lift_rax op ops = 
    let open Op in 
    match ops with
    | [| Imm v |] ->
      let width = dst_width_exn op in
      let width' = src_width_exn op in
      add_rax ~width ~value_width:width' v
    | _ -> raise (Invalid_arg_set op)

  let lift_rm op ops = 
    let open Op in 
    match ops with
    | [|Reg dst; Reg src; Reg base; Imm scale; Reg index; Imm disp; Reg seg|] ->
      let width = dst_width_exn op in
      add_rm ~width dst src base
    | _ -> raise (Invalid_arg_set op)

  let lift_mi op ops = 
    let open Op in
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Imm v |] ->
      let width = dst_width_exn op in
      let value_width = src_width_opt op in
      add_mi ~width ?value_width base v
    | _ -> raise (Invalid_arg_set op)
  
  let lift_mr op ops = 
    let open Op in
    match ops with 
    | [|Reg base; Imm scale; Reg index; Imm disp; Reg seg; Reg src |] ->
      let width = dst_width_exn op in
      add_mr ~width base src
    | _  -> raise (Invalid_arg_set op)

  let lift op ops =
    try
      match op with
      | (#add_ri | #add_ri8 | `ADD64ri32) as op' -> lift_ri op' ops
      | (#add_mi | #add_mi8 | `ADD64mi32) as op' -> lift_mi op' ops
      | #add_rr  as op' -> lift_rr  op' ops
      | #add_rm  as op' -> lift_rm  op' ops
      | #add_mr  as op' -> lift_mr  op' ops
      | #add_rax as op' -> lift_rax op' ops
    with Invalid_arg_set op ->
      Sexp.to_string (Addx_opcode.sexp_of_addx op) |>
      Printf.sprintf "invalid operands set for %s" |>
      invalid_arg

end
