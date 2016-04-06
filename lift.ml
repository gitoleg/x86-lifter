open Core_kernel.Std
open Bap.Std
open Or_error
open X86env
open X86types

module Dis = Disasm_expert.Basic

type lifter = (mem * Dis.full_insn) list -> bil Or_error.t list

let pp_ops fmt =
  Array.iter ~f:(fun op ->
      Format.fprintf fmt "@[%a@ @]" Op.pp op)

let pp_insn fmt insn =
  Format.fprintf fmt "@[%s => %s(%a)@]"
    (Dis.Insn.asm insn)
    (Dis.Insn.name insn)
    pp_ops
    (Dis.Insn.ops insn)

module Lifter (Target : Target) (Env : Env) = struct
  open Target

  module Btx = Btx.Reg(Target.CPU) (Env)
  module Movx = Movx.Reg(Target.CPU) (Env)
  type obil = bil Or_error.t

  let lift mem insn =
    try
      match Decode.opcode insn with
      | Some (#Opcode.btx_reg as op) ->
        Ok (Btx.lift op (Dis.Insn.ops insn))
      | Some (#Opcode.movx as op) ->
        Ok (Movx.lift op (Dis.Insn.ops insn))
      | Some op ->
        Dis.Insn.asm insn |>
        Or_error.errorf "unsupported instruction %s"
      | None -> lift mem insn
    with exn -> Format.asprintf "%a: %a"
                  pp_insn insn
                  Exn.pp exn |>
                Or_error.error_string

  let lift_insns insns : obil list =
    let rec process acc = function
      | [] -> (List.rev acc)
      | (mem,x) :: xs -> match Decode.prefix x with
        | None ->
          let bil = match lift mem x with
            | Error _ as err -> err
            | Ok bil -> Ok bil in
          process (bil::acc) xs
        | Some pre -> match xs with
          | [] ->
            let bil = error "trail prefix" pre Opcode.sexp_of_prefix in
            process (bil::acc) []
          | (mem,y) :: xs ->
            let bil = match lift mem y with
              | Error _ as err -> err
              | Ok bil -> match pre with
                | `REP_PREFIX ->
                  Ok [Bil.(while_ (var CPU.zf) bil)]
                | `REPNE_PREFIX ->
                  Ok [Bil.(while_ (lnot (var CPU.zf)) bil)]
                | `LOCK_PREFIX -> Ok (Bil.special "lock" :: bil)
                | `DATA16_PREFIX -> Ok (Bil.special "data16" :: bil)
                | `REX64_PREFIX -> Ok (Bil.special "rex64" :: bil) in
            process (bil::acc) xs in
    process [] insns
end

let lifter_of_arch arch =
  let module Target =
    (val target_of_arch (arch : Arch.x86 :> arch)) in
  let module Env = (val env_of_arch arch) in
  let module Lifter = Lifter(Target)(Env) in
  Lifter.lift_insns
