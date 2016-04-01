open Core_kernel.Std
open Bap.Std
open Or_error
open Format
open Bap_plugins.Std

module Dis = Disasm_expert.Basic

let null = Addr.of_int64 0L
let arch = `x86_64

let disasm data =
  Bigstring.of_string (String.strip data) |>
  Memory.create LittleEndian null >>= fun mem ->
  Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
      let dis = Dis.store_asm dis |>
                Dis.store_kinds in
      Dis.run dis mem
        ~init:[]
        ~return:ident
        ~stopped:(fun s _ -> Dis.insns s |> Dis.stop s) |>
      List.filter_map ~f:(function
          | (mem,None) ->
            printf "Disasm failed: @.%a@." Memory.pp mem; None
          | (mem,Some insn) -> Some (mem,insn)) |>
      Lift.x64 |>
      List.iter ~f:(function
          | Error err -> printf "Lifter failed: @.%a@." Error.pp err
          | Ok bil -> printf "%a@." Bil.pp bil) |>
      Or_error.return)

let load_plugins () =
  Plugins.load () |>
  List.iter ~f:(function
      | Ok _ -> ()
      | Error (name, err) ->
        printf "Load %s failed: @.%a@.\n" name Error.pp err)

let () =
  at_exit (pp_print_flush err_formatter);
  at_exit (pp_print_flush std_formatter);
  load_plugins ();
  In_channel.input_all stdin |> Scanf.unescaped |> disasm |> function
  | Ok () -> ()
  | Error err -> printf "Program failed with: %a@." Error.pp err;
    exit 1
