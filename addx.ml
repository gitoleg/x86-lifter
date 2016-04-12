open Core_kernel.Std
open Bap.Std
open Types

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

  let define_width width = function
    | None -> width
    | Some w -> w

  let prepare_imm ~width ~value_width value =
    let value_width = define_width width value_width in
    let data = imm_to_exp ~width ~value_width value in
    let v0 = Var.create ~is_virtual:true "v0" (Type.Imm width) in
    Bil.move v0 data, Bil.var v0 

  let add_exp width dst e e' : bil =
    let open Bil.Infix in
    let r = e + e' in
    let flags = set_flags width r e e' in
    flags @ [ dst := r; ]

  let add_rr width op op' : bil =
    add_exp width 
      (var_of_reg width op)
      (exp_of_reg width op)
      (exp_of_reg width op') 

  let add_ri ~width ?value_width dst value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: add_exp width (var_of_reg width dst) (exp_of_reg width dst) data

  let add_mi ~width ?value_width dst_reg value : bil =
    let mv, data = prepare_imm ~width ~value_width value in
    mv :: add_exp width mem_var (exp_of_reg width dst_reg) data

  let add_mr width dst_reg reg : bil =
    let data = load_from_reg width dst_reg in
    add_exp width mem_var (exp_of_reg width dst_reg) data
                                                                
  let add_rm width dst_reg mem_reg : bil =
    let data = load_from_reg width mem_reg in
    add_exp width mem_var (exp_of_reg width dst_reg) data

  let lift op ops =
  let open Op in
    match op,ops with
    | `ADD64rr, [|Reg dst; Reg dst'; Reg r|] -> add_rr 64 dst r
    | `ADD32rr, [|Reg dst; Reg dst'; Reg r|] -> add_rr 32 dst r
    | `ADD16rr, [|Reg dst; Reg dst'; Reg r|] -> add_rr 16 dst r
    | `ADD8rr,  [|Reg dst; Reg dst'; Reg r|] -> add_rr 8  dst r
    | `ADD32ri, [|Reg dst; Reg dst'; Imm v|] -> add_ri ~width:32 dst v
    | `ADD16ri, [|Reg dst; Reg dst'; Imm v|] -> add_ri ~width:16 dst v
    | `ADD8ri,  [|Reg dst; Reg dst'; Imm v|] -> add_ri ~width:8  dst v
    | `ADD64ri8, [|Reg dst; Reg dst'; Imm v|] -> 
      add_ri ~width:64 ~value_width:8 dst v
    | `ADD32ri8, [|Reg dst; Reg dst'; Imm v|]  -> 
      add_ri ~width:32 ~value_width:8 dst v
    | `ADD16ri8, [|Reg dst; Reg dst'; Imm v|] -> 
      add_ri ~width:16 ~value_width:8 dst v
    | `ADD64rm, 
      [|Reg dst; Reg dst'; Reg base; Imm scale; Reg index; Imm disp; Reg seg|] ->
      add_rm 64 dst base
    | `ADD32rm, _ -> failwith "unimplemented ADD32rm"
    | `ADD16rm, _ -> failwith "unimplemented ADD16rm"
    | `ADD8rm,  _ -> failwith "unimplemented ADD8rm"
    | `ADD32mi, _ -> failwith "unimplemented ADD32mi"
    | `ADD16mi, _ -> failwith "unimplemented ADD16mi"
    | `ADD8mi,  _ -> failwith "unimplemented ADD8mi"
    | `ADD64mi8, _ -> failwith "unimplemented ADD64mi8"
    | `ADD32mi8, _ -> failwith "unimplemented ADD32mi8"
    | `ADD16mi8, _ -> failwith "unimplemented ADD16mi8"
    | `ADD64mr, _ -> failwith "unimplemented ADD64mr"
    | `ADD32mr, _ -> failwith "unimplemented ADD32mr"
    | `ADD16mr, _ -> failwith "unimplemented ADD16mr"
    | `ADD8mr, _ -> failwith "unimplemented ADD8mr"
    | `ADD64i32, _ -> failwith "unimplemented ADD64i32"
    | `ADD32i32, _ -> failwith "unimplemented ADD32i32"
    | `ADD16i16, _ -> failwith "unimplemented ADD16i16"
    | `ADD8i8, _ -> failwith "unimplemented ADD8i8"
    | `ADD64mi32, _ -> failwith "unimplemented ADD64mi32"
    | `ADD64ri32, _ -> failwith "unimplemented ADD64ri32"
    | op,ops -> invalid_arg "invalid operation signature"
end
