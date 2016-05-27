open Core_kernel.Std
open Bap.Std
open Or_error
open Format
open Bap_plugins.Std
open Cmdliner

module Dis = Disasm_expert.Basic

let disasm ?on_unsupported arch addr data =
  Bigstring.of_string (String.strip data) |>
  Memory.create LittleEndian addr >>= fun mem ->
  Dis.with_disasm ~backend:"llvm" (Arch.to_string (arch :> arch))
    ~f:(fun dis ->
        let dis = Dis.store_asm dis |>
                  Dis.store_kinds in
        let lift = match arch with
          | `x86 -> Lift.ia32 ?on_unsupported
          | `x86_64 -> Lift.amd64 ?on_unsupported in
        Dis.run dis mem
          ~init:[]
          ~return:ident
          ~stopped:(fun s _ -> Dis.insns s |> Dis.stop s) |>
        List.filter_map ~f:(function
            | (mem,None) ->
              printf "Disasm failed: @.%a@." Memory.pp mem; None
            | (mem,Some insn) -> Some (mem,insn)) |>
        lift |>
        Or_error.return)

let load_plugins () =
  Plugins.load () |>
  List.iter ~f:(function
      | Ok _ -> ()
      | Error (name, err) ->
        printf "Load %s failed: @.%a@.\n" name Error.pp err)

let main arch addr use_bap_lift =
  at_exit (pp_print_flush err_formatter);
  at_exit (pp_print_flush std_formatter);
  load_plugins ();
  let addr = Arch.addr_size (arch :> arch) |>
             Size.in_bits |>
             Printf.sprintf "%s:%d" addr |>
             Addr.of_string in
  let bap_lift =
    let module Target = (val target_of_arch (arch :> arch)) in
    Option.some_if use_bap_lift Target.lift in
  In_channel.input_all stdin |>
  Scanf.unescaped |>
  disasm ?on_unsupported:bap_lift arch addr

let arch: Arch.x86 Term.t =
  let doc = "Target architecture" in
  let arch =
    let parse s =
      match Arch.of_string s with
      | Some (#Arch.x86 as arch) -> `Ok arch
      | Some _ -> `Error ("unsupported architecture " ^ s)
      | None -> `Error ("unknown architecture " ^ s) in
    let print fmt arch = Arch.pp fmt (arch :> arch) in
    parse, print in
  Arg.(value & opt arch `x86_64 & info ["arch"] ~docv:"ARCH" ~doc)

let use_bap_lift =
  let doc = "Use bap lifter to lift unimplemented instructions" in
  Arg.(value & flag & info ["use-bap-lift"] ~doc)

let addr =
  let doc = "Specify an address of first byte, as though \
             the instructions occur at a certain address, \
             and accordingly interpreted. Be careful that \
             you appropriately use 0x prefix for hex and \
             leave it without for decimal." in
  Arg.(value & opt  string "0x0" &  info ["addr"] ~doc)

let cmd =
  let doc = "x86 instruction lifter" in
  let man = [
    `S "DESCTIPTION";
    `P "$(tname) lift binary data from stdin."
  ] in
  Term.(pure main $ arch $ addr $ use_bap_lift),
  Term.info "main.native" ~doc ~man

let _main:unit =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | `Ok result ->
    begin
      match result with
      | Ok data ->
        List.iter ~f:(function
            | Error err -> printf "Lifter failed: %a@." Error.pp err
            | Ok bil -> printf "%a@." Bil.pp bil) data;
        exit 0;
      | Error err ->
        printf "Disasm failed with: %a@." Error.pp err;
        exit 1
    end
  | _ -> exit 0
          
