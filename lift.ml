open Core_kernel.Std
open Bap.Std
open Or_error
open X86env
open X86types

module Dis = Disasm_expert.Basic

type lifter = (mem * Dis.full_insn) list -> bil Or_error.t list

module Lifter (Target : Target) (Env : Env) = struct
  open Target

  module Btx = Btx.Reg(Target) (Env)

  type obil = bil Or_error.t

  let lift mem insn = match Decode.opcode insn with
    | Some (#Opcode.btx_reg as op) ->
      Ok (Btx.lift op (Dis.Insn.ops insn))
    | Some op -> Ok [Bil.special "unsupported instruction"]
    | None -> lift mem insn

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
