open Core_kernel.Std
open Bap.Std
open X86env
open Btx_opcode

module Dis = Disasm_expert.Basic

module Make (CPU : CPU) (Env : X86env.S) (Backend : X86backend.S) =
struct
  open Env
  let zero = Word.of_int64 0L
  let one width = Word.of_int64 ~width 1L

  let (%:) n width = Bil.(int (Word.of_int ~width n))

  let set_cf width x typ off =
    Bil.(CPU.cf := cast low 1 (x lsr (typ width off)))

  let imm width x = match Imm.to_word ~width:8 x with
    | None -> invalid_arg "imm8 must fit into 8 bits"
    | Some x -> Bil.(cast unsigned width (int x mod (width %: 8)))

  let reg width x =
    let x = match width with
      | 32 | 64 -> RR.of_mc x |> Option.value_exn |> RR.get
      | _ -> invalid_arg "Btx.reg : expect (32 | 64)" in
    Bil.(x mod (width %: width))

  let bit width typ off =
    Bil.(int (one width) lsl (typ width off))

  let nothing = None
  let flipped = Some (fun x bit -> Bil.(x lxor bit))
  let one = Some (fun x bit -> Bil.(x lor bit))
  let zero = Some (fun x bit -> Bil.(x land (lnot bit)))

  (* set one 32 r imm x *)
  let set how width reg typ x =
    let exp, set =
      let lhs,rhs = match width with
        | 32 | 64 -> let r = RR.of_mc reg |> Option.value_exn in
          RR.(var r, get r)
        | _ -> invalid_arg "Btx.set: expect (32 | 64)" in
      match how with
      | None -> rhs, []
      | Some set -> rhs, [Bil.(lhs := set rhs (bit width typ x))] in
    set_cf width exp typ x :: set


  let register () =
    let set how width typ xx ops =
      xx ops ~on_error:(Or_error.error_string "unknown operands")
        ~f:(fun b off -> Ok (set how width b typ off)) in
    List.iter ~f:(fun (op, lift) -> Backend.register op lift) [
      `BT64rr, set nothing 64 reg Operand.rr;
      `BT32rr,   set nothing 32 reg Operand.rr;
      `BT16rr,   set nothing 16 reg Operand.rr;
      `BT64ri8,  set nothing 64 imm Operand.ri;
      `BT32ri8,  set nothing 32 imm Operand.ri;
      `BT16ri8,  set nothing 16 imm Operand.ri;
      `BTS64rr,  set one 64 reg Operand.rr;
      `BTS32rr,  set one 32 reg Operand.rr;
      `BTS16rr,  set one 16 reg Operand.rr;
      `BTS64ri8, set one 64 imm Operand.ri;
      `BTS32ri8, set one 32 imm Operand.ri;
      `BTS16ri8, set one 16 imm Operand.ri;
      `BTC64rr,  set flipped 64 reg Operand.rr;
      `BTC32rr,  set flipped 32 reg Operand.rr;
      `BTC16rr,  set flipped 16 reg Operand.rr;
      `BTC64ri8, set flipped 64 imm Operand.ri;
      `BTC32ri8, set flipped 32 imm Operand.ri;
      `BTC16ri8, set flipped 16 imm Operand.ri;
      `BTR64rr,  set zero 64 reg Operand.rr;
      `BTR32rr,  set zero 32 reg Operand.rr;
      `BTR16rr,  set zero 16 reg Operand.rr;
      `BTR64ri8, set zero 64 imm Operand.ri;
      `BTR32ri8, set zero 32 imm Operand.ri;
      `BTR16ri8, set zero 16 imm Operand.ri;
    ]
end

module IA32 = Make (X86_cpu.IA32) (X86env.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86env.AMD64) (X86backend.AMD64)

let register () =
  IA32.register ();
  AMD64.register ()
