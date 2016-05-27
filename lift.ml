open Core_kernel.Std
open Bap.Std
module Dis = Disasm_expert.Basic

let pp_ops fmt =
  Array.iter ~f:(fun op ->
      Format.fprintf fmt "@[%a@ @]" Op.pp op)

let pp_insn fmt insn =
  Format.fprintf fmt "@[%s => %s(%a)@]"
    (Dis.Insn.asm insn)
    (Dis.Insn.name insn)
    pp_ops
    (Dis.Insn.ops insn)

module Make (CPU : CPU) (B:X86backend.S) = struct
  module CPU = CPU

  let lift ?on_unsupported mem insn =
    let ops = Dis.Insn.ops insn in
    Opcode.decode insn |>
    Option.value_map
      ~default:(Format.asprintf "unsupported instruction %a"
                  pp_insn insn |>
                Or_error.error_string)
      ~f:(fun op -> B.lift op mem ops) |> function
    | Ok _ as r -> r
    | Error _ as err -> Option.value_map on_unsupported
                          ~default:err
                          ~f:(fun lifter -> lifter mem insn)

  let lift_insns ?on_unsupported insns =
    let rec process acc = function
      | [] -> (List.rev acc)
      | (mem,x) :: xs -> match Prefix.decode x with
        | None ->
          let bil = match lift ?on_unsupported mem x with
            | Error _ as err -> err
            | Ok bil -> Ok bil in
          process (bil::acc) xs
        | Some pre -> match xs with
          | [] ->
            let bil = error "trail prefix" pre Prefix.sexp_of_t in
            process (bil::acc) []
          | (mem,y) :: xs ->
            let bil = match lift ?on_unsupported mem y with
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

module IA32 = Make (X86_cpu.IA32) (X86backend.IA32)
module AMD64 = Make (X86_cpu.AMD64) (X86backend.AMD64)


let () =
  List.iter ~f:(fun m ->
      let module M = (val m : X86backend.R) in
      M.register ())
    [ (module Mov : X86backend.R);
      (module Btx : X86backend.R);
      (module Cmpxchg : X86backend.R) ]


type x86_lifter =
  (mem * Disasm_expert.Basic.full_insn) list ->
  bil Or_error.t list

let ia32 = IA32.lift_insns
let amd64 = AMD64.lift_insns
