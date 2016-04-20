open Core_kernel.Std 
open Bap.Std
open Addx_opcode
open Asm.Reg


module Make  (CPU : CPU) (Env : X86env.S) (Lifters : X86backend.S) = struct

  open Env
      
  let msb r = Bil.(cast high 1 r)
  let set_nf res = Bil.(CPU.nf := msb res)

  let set_vf res op op' =
    Bil.(CPU.vf := msb (lnot (op lxor op')) land (op lxor res))

  let set_zf width res =
    Bil.(CPU.zf := res = int (Word.zero width))

  let set_nvzf width res op op' = 
    [set_vf res op op';
     set_nf res;
     set_zf width res;]

  let set_flags width res op op' =
    Bil.(CPU.cf := res < op) :: set_nvzf width res op op'

  let exp_of_imm ~width ~value_width imm = 
    let imm = IM.of_imm imm in
    let exp = IM.get ~width:value_width imm in
    if width = value_width then exp
    else
      Bil.(cast signed (Size.in_bits width) exp)

  let add_exp size op op' =
    let width = Size.in_bits size in
    let res = Bil.(op + op') in
    let flags = set_flags width res op op' in
    res, flags

  let add size dst op op' = 
    let res, flags = add_exp size op op' in
    Ok (RR.set dst res :: flags)

  let add_rr (op : add_rr) ops =
    Operand.rrr ops ~f:(fun dst src src' ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        let src' = RR.of_mc_exn src' in
        add (RR.width dst) dst (RR.get src) (RR.get src'))

  let add_ri_basic dst src imm value_width = 
    let dst = RR.of_mc_exn dst in
    let src = RR.of_mc_exn src in
    let width = RR.width dst in
    let imm = exp_of_imm ~width ~value_width imm in
    add width dst (RR.get src) imm

  let add_ri (op : add_ri) ops = 
    Operand.rri ops ~f:(fun dst src imm ->
        let width = RR.width (RR.of_mc_exn dst) in
        add_ri_basic dst src imm width)

  let add_ri_width (op : add_ri_width) ops = 
    Operand.rri ops ~f:(fun dst src imm ->
        let value_size = 
          match op with 
          | `ADD64ri8 -> `r8
          | `ADD32ri8 -> `r8
          | `ADD16ri8 -> `r8
          | `ADD64ri32 -> `r32 in
        add_ri_basic dst src imm value_size)   

  let add_rm (op : add_rm) ops = 
    Operand.rrm ops ~f:(fun dst src mem ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        let mem = MM.of_mem mem in
        let size = RR.width dst in
        let mem_exp = MM.load mem ~size in
        add size dst (RR.get src) mem_exp)
     
  let add_mr (op : add_mr) ops = 
    Operand.mr ops ~f:(fun mem src ->
        let mem = MM.of_mem mem in
        let src = RR.of_mc_exn src in
        let size = RR.width src in
        let mem_src = MM.load mem ~size in
        let res, flags = add_exp size mem_src (RR.get src) in
        Ok (MM.store mem ~size res :: flags))
      
  let add_mi_basic mem imm width value_width = 
    let mem = MM.of_mem mem in
    let imm = exp_of_imm ~width ~value_width imm in
    let mem_src = MM.load mem ~size:width in 
    let res, flags = add_exp width mem_src imm in
    Ok (MM.store mem ~size:width res :: flags)

  let add_mi  (op : add_mi) ops = 
    Operand.mi ops ~f:(fun mem imm ->
        let width = 
          match op with 
          | `ADD32mi -> `r32
          | `ADD16mi -> `r16 
          | `ADD8mi  -> `r8 in
        add_mi_basic mem imm width width)
  
  let add_mi_width (op : add_mi_width) ops = 
    Operand.mi ops ~f:(fun mem imm ->
        let width, value_width = 
          match op with 
          | `ADD64mi8 -> `r64, `r8
          | `ADD32mi8 -> `r32, `r8
          | `ADD16mi8 -> `r16, `r8
          | `ADD64mi32 -> `r64, `r32 in
        add_mi_basic mem imm width value_width)

  let add_rax (op : add_rax) ops =
    Operand.i ops ~f:(fun imm ->
        let dst, value_width = 
          match op with
          | `ADD64i32 -> `RAX, `r32
          | `ADD32i32 -> `EAX, `r32
          | `ADD16i16 -> `AX, `r16
          | `ADD8i8 -> `AL, `r8 in
        let dst' = RR.of_asm_exn dst in
        let width = RR.width dst' in
        let imm = exp_of_imm ~width ~value_width imm in
        add width dst' (RR.get dst') imm)

  let addx (op : Addx_opcode.t) mem ops =
    Or_error.try_with_join (fun () ->
        match op with
        | #add_rr as op -> add_rr op ops
        | #add_ri as op -> add_ri op ops
        | #add_ri_width as op -> add_ri_width op ops
        | #add_rm as op -> add_rm op ops
        | #add_mr as op -> add_mr op ops
        | #add_mi as op -> add_mi op ops
        | #add_mi_width as op -> add_mi_width op ops
        | #add_rax as op -> add_rax op ops)

  let register () =
    List.iter ~f:(fun op -> Lifters.register (op :> Opcode.t) (addx op))
      Opcode.all_of_addx

end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64) (X86backend.AMD64)

let register () =
  IA32.register ();
  AMD64.register ()

      
