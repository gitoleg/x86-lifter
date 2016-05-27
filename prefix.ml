open Core_kernel.Std
open Bap.Std
open Regular.Std

type t = [
  | `LOCK_PREFIX
  | `REX64_PREFIX
  | `DATA16_PREFIX
  | `REP_PREFIX
  | `REPNE_PREFIX
] [@@deriving bin_io, sexp, compare, enumerate]

module T = struct
  open Format

  type nonrec t = t [@@deriving bin_io, compare, sexp]
  let module_name = Some "Decode"
  let version = "0.1"
  let pp fmt t =
    fprintf fmt "%s" (sexp_of_t t |> Sexp.to_string)

  let hash = Hashtbl.hash
end

include Regular.Make(T)

let decode insn =
  Option.try_with (fun () ->
    Disasm_expert.Basic.Insn.name insn |>
    Sexp.of_string |>
    t_of_sexp)

module Lifter (Env : X86env.S) = struct
  type 'a p = 'a constraint 'a = [< Opcode.t]
  type rep = [
    | Opcode.ins
    | Opcode.lods
    | Opcode.movs
    | Opcode.outs
    | Opcode.stos] p

  type repe = [
    | Opcode.cmps
    | Opcode.scas] p

  type repne = repe

  type lock = Opcode.t p (*FIXME: The LOCK prefix can only be used
                           with forms of the following instructions
                           that write a memory operand: ADC, ADD, AND,
                           BTC, BTR, BTS, CMPXCHG, CMPXCHG8B,
                           CMPXCHG16B, DEC, INC, NEG, NOT, OR, SBB,
                           SUB, XADD, XCHG, and XOR. An invalid-opcode
                           exception occurs if the LOCK prefix is used
                           with any other instruction.*)

  open Env

  module Rep = struct
    let rcx =
      let reg = match RR.size with
        | `r32 -> `ECX
        | `r64 -> `RCX in
      RR.of_asm_exn reg |> RR.var

    let size = Size.in_bits RR.size

    let dec_rcx = Bil.(rcx := var rcx - int (Word.one size))
    let rcx_cond = Bil.(var rcx <> int (Word.zero size))

    let lift_rep bil =
      let body = List.concat [bil; [dec_rcx]] in
      [Bil.(while_ rcx_cond body)]

    let lift_repe bil =
      let zf = FR.get `ZF in
      let flag = Var.create "flag" bool_t in
      let body = List.concat [bil; [dec_rcx; Bil.(flag := zf)]] in
      [Bil.(flag := int Word.b1);
       Bil.(while_ (rcx_cond land (var flag)) body)]

    let lift_repne bil =
      let zf = FR.get `ZF in
      let flag = Var.create "flag" bool_t in
      let body = List.concat [bil; [dec_rcx; Bil.(flag := zf)]] in
      [Bil.(flag := int Word.b0);
       Bil.(while_ (rcx_cond land lnot (var flag)) body)]
  end

  let lift (prefix:t) opcode bil =
    match prefix, opcode with
    | `LOCK_PREFIX, Some #lock -> Ok (Bil.special "lock" :: bil)
    | `LOCK_PREFIX, _ -> Ok (Bil.special "lock" :: bil)
    | `REX64_PREFIX, _ -> Ok (Bil.special "rex64" :: bil)
    | `DATA16_PREFIX, _ -> Ok (Bil.special "data16" :: bil)
    | `REP_PREFIX, Some #rep -> Ok (Rep.lift_rep bil)
    | `REP_PREFIX, Some #repe -> Ok (Rep.lift_repe bil)
    | `REPNE_PREFIX, Some #repne -> Ok (Rep.lift_repne bil)
    | _ -> Or_error.errorf "usupported prefix %s with instruction %s"
             (sexp_of_t prefix |> Sexp.to_string)
             (Option.sexp_of_t Opcode.sexp_of_t opcode |> Sexp.to_string)

end
