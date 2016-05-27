open Core_kernel.Std
open Bap.Std
open X86env
open Btx_opcode

module Make (Env : X86env.S) = struct
  open Env
  let set_flags op base off =
    let op = match op with
      | #bt -> "bt"
      | #btc -> "btc"
      | #btr -> "btr"
      | #bts -> "bts" in
    FR.set `CF Bil.(cast low 1 (base lsr off)) ::
    List.map ~f:(fun f -> FR.set_unknown f op) [`OF; `SF; `ZF; `AF; `PF]

  let offset_width = function
    | `r16 -> 4
    | `r32 -> 5
    | `r64 -> 6
    | _ -> invalid_arg "Btx: offset width"

  let local_var name width =
    Var.create ~is_virtual:true ~fresh:true name @@ Type.imm width

  let one size = Size.in_bits size |> Word.one

  let mask_imm size o () = Word.lshift (one size) o |> Bil.int

  let mask_reg size o () = Bil.(int (one size) lsl var o)

  let side_effect op data (mask:unit -> exp) (stmt: exp -> stmt) =
    match op with
    | #bt -> []
    | #btc -> [stmt Bil.(data lxor mask ())]
    | #btr -> [stmt Bil.(data land lnot (mask ()))]
    | #bts -> [stmt Bil.(data lor mask ())]

  let btx_rr (op:btx_rr) _mem ops =
    Operand.rr ops ~f:(fun base index ->
        let base = RR.of_mc_exn base in
        let index = RR.of_mc_exn index in
        let size = RR.width base in
        let w =  offset_width size in
        let o = local_var "o" w in
        let base = RR.var base in
        let index = RR.var index in
        let bil =
          let off = Bil.(o := cast low w (var index)) in
          let flag = set_flags op (Bil.var base) (Bil.var o) in
          let side = side_effect op (Bil.var base)
              (mask_reg RR.size o)
              (fun exp -> Bil.(base := exp)) in
          List.concat [off::flag; side] in
        Ok bil)

  let btx_ri (op:btx_ri) _mem ops =
    Operand.ri ops ~f:(fun base index ->
        let base = RR.of_mc_exn base in
        let size = RR.width base in
        let o = Imm.to_word index ~width:(offset_width size) |>
                Option.value_exn in
        let base = RR.var base in
        let bil =
          let flags = set_flags op (Bil.var base) (Bil.int o) in
          let side = side_effect op (Bil.var base)
              (mask_imm RR.size o)
              (fun exp -> Bil.(base := exp)) in
          List.concat [flags; side] in
        Ok bil)

  let btx_mi (op:btx_mi) mem ops =
    Operand.mi ops ~f:(fun base index ->
        let base = MM.of_mem mem base in
        let size = match op with
          | `BT64mi8 | `BTC64mi8 | `BTR64mi8 | `BTS64mi8 -> `r64
          | `BT32mi8 | `BTC32mi8 | `BTR32mi8 | `BTS32mi8 -> `r32
          | `BT16mi8 | `BTC16mi8 | `BTR16mi8 | `BTS16mi8 -> `r16 in
        let a = local_var "a" @@ Size.in_bits MM.addr_size in
        let d = local_var "d" @@ Size.in_bits size in
        let o = Imm.to_word index ~width:(offset_width size) |>
                Option.value_exn in
        let bil =
          let load = [
            Bil.(a := MM.addr base);
            Bil.(d := MM.load_from ~size (var a))] in
          let flag = set_flags op (Bil.var d) (Bil.int o) in
          let side = side_effect op (Bil.var d)
              (mask_imm size o)
              (fun exp -> MM.store_to Bil.(var a) ~size exp) in
          List.concat [load; flag; side] in
        Ok bil)

  let btx_mr (op:btx_mr) mem ops =
    Operand.mr ops ~f:(fun base index ->
        let base = MM.of_mem mem base in
        let index = RR.of_mc_exn index in
        let size = RR.width index in
        let bsize = Size.in_bits size in
        let bsize_addr = Size.in_bits MM.addr_size in
        let a = local_var "a" bsize_addr in
        let b = local_var "b" bsize_addr in
        let d = local_var "d" bsize in
        let w = offset_width size in
        let o = local_var "o" w in
        let f = Word.of_int ~width:bsize (Size.in_bytes size) in
        let dv = Word.of_int ~width:bsize bsize in
        let bil =
          let load =
            let bexp =
              let e = Bil.(int f * (RR.get index /$ int dv)) in
              if bsize_addr <> bsize
              then Bil.(cast signed bsize_addr e)
              else e in
            [ Bil.(a := MM.addr base);
              Bil.(b := bexp);
              Bil.(d := MM.load_from ~size (var a + var b));] in
          let flags =
            Bil.(o := cast low w (var (RR.var index))) ::
            set_flags op (Bil.var d) (Bil.var o) in
          let side = side_effect op (Bil.var d)
              (mask_reg size o)
              (fun exp -> MM.store_to Bil.(var a + var b) ~size exp) in
          List.concat [load; flags; side] in
        Ok bil)

  let btx (op:Btx_opcode.t) =
    match op with
    | #btx_rr as op -> btx_rr op
    | #btx_ri as op -> btx_ri op
    | #btx_mi as op -> btx_mi op
    | #btx_mr as op -> btx_mr op
end

module IA32 = Make (X86env.IA32)
module AMD64 = Make (X86env.AMD64)

let register () =
  List.iter (all_of_btx_ia32 :> Btx_opcode.t list) ~f:(fun op ->
      X86backend.IA32.register (op :> Opcode.t) (IA32.btx op));
  List.iter (all_of_btx :> Btx_opcode.t list) ~f:(fun op ->
      X86backend.AMD64.register (op :> Opcode.t) (AMD64.btx op));
