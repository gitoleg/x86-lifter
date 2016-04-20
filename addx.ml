open Core_kernel.Std 
open Bap.Std
open Asm.Reg
open Addx_opcode

let string_of_opcode op = 
  Sexp.to_string (Addx_opcode.sexp_of_t64 op)

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

let search_size_exn ~index op = 
  Size.of_int_exn (search_number_exn ~index op)

let search_size_opt ~index op = 
  Option.(search_number_opt ~index op >>= fun w -> 
          Size.of_int_opt w)

let tmp name width = 
  Var.create ~fresh:true ~is_virtual:true name (Type.Imm width)

module Make (CPU : CPU) (Env : X86env.S) = struct

  open Env

  let msb r = Bil.(cast high 1 r)
  let set_nf res = Bil.(CPU.nf := msb res)
  let set_cf res op = Bil.(CPU.cf := res < op)

  let set_vf res op op' =
    Bil.(CPU.vf := msb (lnot (op lxor op')) land (op lxor res))

  let set_zf width res =
    Bil.(CPU.zf := res = int (Word.zero width))

  let set_flags width res op op' = 
    [ set_cf res op;
      set_vf res op op';
      set_nf res;
      set_zf width res;]

  (** [prepare_imm ~width ~value_width imm] - creates a 
      temporary variable associated with [imm] and returns
      a stmt and exp for it *)
  let prepare_imm ~width ~value_width imm = 
    let imm = IM.of_imm imm in
    let exp = IM.get ~width:value_width imm in
    let v0 = tmp "v0" (Size.in_bits width) in
    if width = value_width then Bil.move v0 exp, Bil.var v0
    else
      let exp' = Bil.(cast signed (Size.in_bits width) exp) in
      Bil.move v0 exp', Bil.var v0

  let add_exp size op op' =
    let width = Size.in_bits size in
    let res = Bil.(op + op') in
    let flags = set_flags width res op op' in
    res, flags

  let add size dst op op' = 
    let res, flags = add_exp size op op' in
    Ok (RR.set dst res :: flags)

  let add_rr op ops =
    Operand.rrr ops ~f:(fun dst src src' ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        let src' = RR.of_mc_exn src' in
        add (RR.width dst) dst (RR.get src) (RR.get src'))

  let add_ri_basic dst src imm value_width = 
    let dst = RR.of_mc_exn dst in
    let src = RR.of_mc_exn src in
    let width = RR.width dst in
    let mv, imm_exp = prepare_imm ~width ~value_width imm in
    let res, flags = add_exp width (RR.get src) imm_exp in
    Ok (mv :: RR.set dst res :: flags)

  let add_ri op ops = 
    Operand.rri ops ~f:(fun dst src imm ->
        let width = RR.width (RR.of_mc_exn dst) in
        add_ri_basic dst src imm width)

  let add_ri_width op ops = 
    Operand.rri ops ~f:(fun dst src imm ->
        let value_size = search_size_exn ~index:1 op in
        add_ri_basic dst src imm value_size)   

  let add_rm op ops = 
    Operand.rrm ops ~f:(fun dst src mem ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        let mem = MM.of_mem mem in
        let size = RR.width dst in
        let mem_exp = MM.load mem ~size in
        add size dst (RR.get src) mem_exp)

  let add_mr op ops = 
    Operand.mr ops ~f:(fun mem src ->
        let mem = MM.of_mem mem in
        let src = RR.of_mc_exn src in
        let size = RR.width src in
        let mem_src = MM.load mem ~size in
        let res, flags = add_exp size mem_src (RR.get src) in
        Ok (MM.store mem ~size res :: flags))

  let add_mi_basic mem imm width value_width = 
    let mem = MM.of_mem mem in
    let mem_src = MM.load mem ~size:width in 
    let mv, imm_exp = prepare_imm ~width ~value_width imm in
    let res, flags = add_exp width mem_src imm_exp in
    Ok (mv :: MM.store mem ~size:width res :: flags)

  let add_mi op ops = 
    Operand.mi ops ~f:(fun mem imm ->
        let width = search_size_exn ~index:0 op in
        add_mi_basic mem imm width width)

  let add_mi_width op ops = 
    Operand.mi ops ~f:(fun mem imm ->
        let width = search_size_exn ~index:0 op in
        let value_width = search_size_exn ~index:1 op in
        add_mi_basic mem imm width value_width)

  let add_rax op ops =
    Operand.i ops ~f:(fun imm ->
        let dst, value_width = 
          match op with
          | `ADD64i32 -> `RAX, `r32
          | `ADD32i32 -> `EAX, `r32
          | `ADD16i16 -> `AX, `r16
          | `ADD8i8   -> `AL, `r8 in
        let dst' = RR.of_asm_exn dst in
        let width = RR.width dst' in
        let mv, imm_exp = prepare_imm ~width ~value_width imm in
        let res, flags = add_exp width (RR.get dst') imm_exp in
        Ok (mv :: RR.set dst' res :: flags))

  let addx (op : Addx_opcode.t64) mem ops =
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
end

module Add64 = struct
  include Make (X86_cpu.AMD64) (X86env.AMD64)
  module Backend = X86backend.AMD64

  let register () = 
    List.iter ~f:(fun op -> Backend.register (op :> Opcode.t) (addx op))
      Opcode.all_of_addx64
end

module Add32 = struct
  include Make (X86_cpu.IA32) (X86env.IA32)
  module Backend = X86backend.IA32

  let addx (op : Addx_opcode.t32) mem ops =
    addx (op :> Addx_opcode.t64) mem ops

  let register () = 
    List.iter ~f:(fun op -> Backend.register (op :> Opcode.t) (addx op))
      Opcode.all_of_addx32
end
